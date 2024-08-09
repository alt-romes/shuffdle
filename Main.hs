{-# LANGUAGE OverloadedRecordDot, RecordWildCards, LambdaCase, TypeApplications, ViewPatterns #-}

import Debug.Trace
import System.Exit
import System.Environment
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
import qualified Data.List as List
import Text.Read (readMaybe)
import System.Random
import System.Random.Stateful
import Control.Monad
import Data.Maybe
import Data.Ord
import Data.Tuple
import Data.Bifunctor
import System.Timeout
import Data.Tree
import qualified Data.List.NonEmpty as NE
import Data.Function

move :: Int {- index -} -> Direction -> Board -> Maybe Board
move ix dir Board{size, tiles} = do
  tgt <- target_ix
  guard (target_is_empty tgt)
  return Board{size, tiles=upd_board tgt}
  where
    col_ix = ix `mod` size
    upd_board tgt = IM.insert ix Empty $
                    IM.insert tgt (chLetter <$> (tiles IM.! ix) {- advance to the next letter on every move -})
                      tiles
    chLetter
      | dir == D || dir == R
      = nextLetter
      | otherwise
      = previousLetter
    target_is_empty = (== Empty) . (tiles IM.!)
    target_ix = case dir of
      U -> do
        guard (ix - size >= 0)
        return @Maybe (ix - size)
      R -> do
        guard (col_ix + 1 < size)
        return (ix + 1)
      D -> do
        guard (ix + size < size*size)
        return (ix + size)
      L -> do
        guard (col_ix - 1 >= 0)
        return (ix - 1)

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p a = do
  x <- a
  if p x then return x else untilM p a

type SolutionPick = (Bool, Bool, Int) -- pickRow, pickStraight, line_ix

generateBoard :: StatefulGen g m => Bool {- easy mode -} -> String {- N letter word -} -> g -> m (Board, Int, SolutionPick)
generateBoard easy word gen = do
  let
    size = length word
    nTiles {- total number of tiles -}
      = size*size

  (solutionIxs, pick) <- do
    pick@(pickRow, pickStraight, line_ix)
      <-
        if easy then
           return (True, True, size-1)
        else do
          -- Row vs Column
          pickRow <- uniformM gen
          -- Straight vs Reverse
          -- Nevermind! Don't allow reversed words, it becomes too masochistic for a game.
          pickStraight <- {- uniformM gen -} pure True
          -- Pick Row or Col Ix
          line_ix <- uniformRM (0, size-1) gen
          return (pickRow, pickStraight, line_ix)
    (,pick) <$>
      case (pickRow, pickStraight) of
        (True, True) ->
          return $ take size [line_ix*size..]
        (True, False) ->
          return $ take size [size-1+line_ix*size,size-1+line_ix*size-1..]
        (False, True) ->
          return $ take size [line_ix,line_ix+size..]
        (False, False) ->
          return $ take size [line_ix+size*(size-1),line_ix-size..]

  holesIxs <- replicateM size (untilM (not . (`List.elem` solutionIxs)) (uniformRM (0, nTiles - 1) gen))

  fullLetters <- IM.fromList <$> mapM (\i -> ((i,) . With) <$> uniformRM ('A', 'Z') gen) [0..nTiles-1]

  let
    solutionRow = IM.fromList $ zip solutionIxs (map With word)
    holes = IM.fromList $ map (,Empty) holesIxs

    solutionBoard = solutionRow `IM.union` holes `IM.union` fullLetters {- union is left biased -}

    loop make_row_empty b nm extra_moves_count
      -- On hard mode, first make the solution row empty, then do N extra moves
      -- to mix it up.
      -- On easy mode, stop when the row is empty.
      | make_row_empty && rowIsEmpty solutionIxs b
      = if easy then do
          pure (b.tiles, nm)
        else do
          loop False b nm 50000 {- start extra move count -}
      | not make_row_empty && extra_moves_count <= 0
      = pure (b.tiles, nm)
      | otherwise
      = do (ix, mov) <- genMove b gen
           let (b',nm') = maybe (b,nm) (,nm+1) $ move ix mov b
           loop make_row_empty b' nm' (extra_moves_count - 1)

  (initialBoard, nMoves) <- loop True Board{size, tiles=solutionBoard} 0 0

  return (Board{size, tiles=initialBoard}, nMoves, pick)

-- | Tries to be smarter and generates only moves to holes, which should be all valid.
genMove :: StatefulGen g m => Board -> g -> m (Int, Direction)
genMove b gen = do
  let holes = getHoles b
  holeIx <- uniformRM (0,length holes - 1) gen
  let adjacents = getAdjacent (holes !! holeIx) b
  if length adjacents > 0 then do
    adjIx  <- uniformRM (0,length adjacents - 1) gen
    let (adj, dirToAdj) = adjacents !! adjIx
    return (adj, flipDir dirToAdj {- get dir from adj to hole -})
  else do
    -- No valid filled adjacent tiles. Try again.
    genMove b gen

possibleMoves :: Board -> [(Int, Direction)] {-^ a tile that can move and a direction it can move in -}
possibleMoves b =
  [ (adj, flipDir dirToAdj) | h <- getHoles b, (adj, dirToAdj) <- getAdjacent h b ]


-- First board I ever solved! Or second. Word=REFER.
sampleBoard = [
  [With 'F', With 'U', With 'U', With 'M', With 'F'],
  [With 'Y', With 'I', With 'M', With 'J', With 'Z'],
  [With 'E', With 'C', With 'X', With 'S', With 'F'],
  [With 'Q', With 'R', With 'W', With 'D', With 'M'],
  [Empty, Empty, Empty, Empty, Empty]
              ]
main = do
    let w = "REFER"
    -- print $ length $ (levels $ (puzzleSearchSpace (boardFromRows sampleBoard))) !! 10
    timeout 10_000_000 $ do
      print $ solve w $ annotateCosts w $ puzzleSearchSpace (boardFromRows sampleBoard)
    exitWith ExitSuccess
    ls <- lines <$> readFile "wordle-La.txt"
    wordIx <- randomRIO (0, length ls - 1)
    let word = ls !! wordIx
    putStrLn $ "target-word:" ++ word
    let tryGenBoard = do
          -- After 10s kill the generation
          res <- timeout (10*1000*1000) $ do
            -- Must be forced otherwise the time will be spent after taking from the MVar!
            ((!b,!m, !p), g') <- flip runStateGen (generateBoard True word) <$> initStdGen
            (!hard_b,!hard_m, !hard_p) <- flip runStateGen_ (generateBoard False word) <$> pure g'
            return ((b,m,p), (hard_b,hard_m,hard_p))
          case res of
            Nothing -> do
              putStrLn "Retrying..."
              tryGenBoard
            Just x  -> return x
    ((board, nMoves, pick), (hard_board, hard_nMoves, hard_pick)) <- tryGenBoard
    putStrLn $ "board-id:" ++ boardId board
    putStrLn $ "Constructing the solution took " ++ show nMoves ++ " moves"
    putStrLn $ "Solution pos " ++ show pick

    putStrLn $ "hard-board-id:" ++ boardId hard_board
    putStrLn $ "Constructing the hard board took " ++ show hard_nMoves ++ " moves"
    putStrLn $ "Hard solution pos " ++ show hard_pick

  where
  loop game = do
    print game
    inp <- getLine
    case readMaybe @(Int, Direction) inp of
      Nothing -> loop game
      Just (ix, dir) -> loop $ fromMaybe game $ move ix dir game

--------------------------------------------------------------------------------
-- * Solver

data Move = Move Int Direction
          | NoMove -- ^ For the starting board
          deriving (Eq, Show)

type Cost = Int

puzzleSearchSpace :: Board -> Tree (Board, Move)
puzzleSearchSpace board = go (board, NoMove) where
  go (b,m) = Node (b, m) (map go $ nextBoards board)

annotateCosts :: String -> Tree (Board, Move) -> Tree (Board, Move, Cost)
annotateCosts sol = go 0 where
  go pathCost (Node (b,m) ns) =
    let h = costToWin sol b
        g = pathCost
     in Node (b, m, g + h) (map (go (g+h)) ns)

nextBoards :: Board -> [(Board, Move)]
nextBoards b = [ (b', Move i d) | (i, d) <- possibleMoves b, Just b' <- [move i d b] ]

costToWin :: String -> Board -> Cost
costToWin sol Board{size,tiles} =
  fixedCost + minimum varCosts
  where
   varCosts = do
     ls <- forM (IM.toList multiOpt) $ \(k, vars) -> do
       (cost, tile) <- NE.toList vars
       pure @[] (cost, tile)
     guard $ length ls == length (List.nubBy ((==) `on` snd) ls)
     pure $ foldr ((+) . fst) 0 ls
   fixedCost = IM.foldr ((+) . fst . NE.head) 0 singleOpt
   (singleOpt, multiOpt) = IM.partition ((== 1) . NE.length) costMap
   costMap =
    IM.fromListWith (<>)
    [
      (s, NE.singleton (m, ix)) -- for stride s, piece ix is solution in m moves

    | (ix,tile) <- IM.toList tiles

    , let (row_ix, col_ix) = ix `divMod` size

    -- Trivial guards
    , row_ix < 4
    , With c <- [tile]

    , let h = col_ix            -- horiz dist to first col
          v = size - 1 - row_ix -- vert dist to last row
    , s <- [0..4]               -- col stride into sol

    -- Value the piece would have in the sol at this stride
    , let vs = toEnum @Char (fromEnum c - h + v + s)

    -- Guard is solution
    , vs == sol !! s

    -- Number of moves to get there
    , let m = abs (h - s) + v
    ]

solve :: String -> Tree (Board, Move, Cost) -> Maybe [Move]
solve sol init = idaStar where

  idaStar =
    dfid (bestFirst init) [100,200..]
      where
        bestFirst (Node b bs) =
          Node b $
            map bestFirst $
              List.sortOn (\(Node (_,_,c) _) -> c) bs

  dfid problem cutoffs
    = case mapMaybe (\cutoff -> dfs 0 cutoff [] problem) cutoffs of
        [] -> Nothing
        (firstResult:_) -> Just firstResult

  dfs !d cutoff mvs (Node (b,mv,cost) bs)
    | checkEasyWin sol b
    = Just (mv:mvs)
    | cost >= cutoff || d == 50
    = Nothing
    | otherwise
    = case mapMaybe (dfs (d+1) cutoff (mv:mvs)) bs of
        []  -> Nothing
        x:_ -> Just x

checkEasyWin :: String -> Board -> Bool
checkEasyWin word Board{size, tiles} =
  map With word == map (tiles IM.!) ixs
    where
      ixs = [size*(size-1)..size*size-1]

--------------------------------------------------------------------------------
-- * Board

data Board = Board
  { size :: Int -- ^ size of one row or column (N)
  , tiles :: IM.IntMap (Tile Char) -- ^ NxN board
  }
  deriving Eq

data Tile a = Empty | With a deriving (Eq, Functor)

data Direction = U | R | D | L deriving (Show, Read, Eq, Bounded, Enum)

boardFromRows :: [[Tile Char]] -> Board
boardFromRows rows = Board row_size $ IM.fromList $ concat $ zipWith go rows [0..] where
  go row row_ix = zip [row_size*row_ix..] row
  row_size = maybe 0 (length . fst) (List.uncons rows)

boardToRows :: Board -> [[Tile Char]]
boardToRows Board{size, tiles} = go 0 0 [] [] where
  go row col row_acc col_acc
    | row == size && col == size
    = reverse row_acc
    | col == size
    = go (row+1) 0 (reverse col_acc:row_acc) []
    | otherwise
    = go row (col+1) row_acc (tiles IM.! (row*size+col) : col_acc)

displayBoard :: Board -> String
displayBoard = List.intercalate "\n" . map (unwords . map show) . boardToRows

boardId :: Board -> String
boardId Board{tiles} = show $ map snd $ IM.toList tiles

rowIsEmpty :: [Int] -> Board -> Bool
rowIsEmpty ixs Board{size, tiles} =
  all (==Empty) $ map (tiles IM.!) ixs

-- | Returns the indices of all holes in the board
getHoles :: Board -> [Int]
getHoles Board{tiles} = IM.keys $ IM.filter (== Empty) tiles

-- | Down to up, left to right, and vice versa
flipDir :: Direction -> Direction
flipDir U = D
flipDir D = U
flipDir R = L
flipDir L = R

-- | Where do we go if we follow this direction?
applyDir :: Board -> Int -> Direction -> Int
applyDir Board{size} i U = i - size
applyDir Board{size} i D = i + size
applyDir Board{} i R = i + 1
applyDir Board{} i L = i - 1

-- | Get adjacent tiles and the direction to which the given tile needs to move
-- to get to the returned neighbour. The neighbour must not be empty.
getAdjacent :: Int -> Board -> [(Int, Direction)]
getAdjacent ix b@Board{size, tiles} = catMaybes $
  flip map
    [(applyDir b ix L, L), (applyDir b ix R, R), (applyDir b ix D, D), (applyDir b ix U, U)] $ \(tgt, d) -> do
    case IM.lookup tgt tiles of
      Nothing    -> Nothing
      Just Empty -> Nothing
      Just _     -> Just (tgt, d)

--------------------------------------------------------------------------------
-- * Utils

-- | Advance a character to the next alphabet letter (e.g. A -> B, D -> E)
nextLetter :: Char -> Char
nextLetter c =
  if succ c > 'Z' then 'A'
                  else succ c

previousLetter :: Char -> Char
previousLetter c =
  if pred c < 'A' then 'Z'
                  else pred c

--------------------------------------------------------------------------------
-- * Instances

instance Show a => Show (Tile a) where
  show Empty = "'_'"
  show (With x) = show x

instance Show Board where
  show = displayBoard

instance UniformRange Direction where
  uniformRM (a,b) g = toEnum <$> uniformRM (fromEnum a, fromEnum b) g

--------------------------------------------------------------------------------
-- * Test

roundTripBoard a = a == (boardToRows . boardFromRows) a

