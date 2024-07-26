{-# LANGUAGE OverloadedRecordDot, RecordWildCards, LambdaCase, TypeApplications, ViewPatterns #-}

import System.Environment
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.IntMap as IM
-- import qualified Data.IntSet as IS
import qualified Data.List as List
import Text.Read (readMaybe)
import System.Random
import System.Random.Stateful
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Tuple
import Data.Bifunctor

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
      = do ix <- uniformRM (0,size*size-1) gen
           mov <- uniformRM (minBound @Direction, maxBound @Direction) gen
           let (b',nm') = maybe (b,nm) (,nm+1) $ move ix mov b
           loop make_row_empty b' nm' (extra_moves_count - 1)

  (initialBoard, nMoves) <- loop True Board{size, tiles=solutionBoard} 0 0

  return (Board{size, tiles=initialBoard}, nMoves, pick)

sampleBoard = [
  [With 'F', With 'U', With 'U', With 'M', With 'F'],
  [With 'Y', With 'I', With 'M', With 'J', With 'Z'],
  [With 'E', With 'C', With 'X', With 'S', With 'F'],
  [With 'Q', With 'R', With 'W', With 'D', With 'M'],
  [Empty, Empty, Empty, Empty, Empty]
              ]
main = do
    ls <- lines <$> readFile "wordle-La.txt"
    wordIx <- randomRIO (0, length ls - 1)
    let word = ls !! wordIx
    putStrLn $ "target-word:" ++ word
    boardSyn <- newEmptyMVar
    isDone   <- newMVar False
    let tryGenBoard = do
          genTid <- forkIO $ do
            -- Must be forced otherwise the time will be spent after taking from the MVar!
            ((!b,!m, !p), g') <- flip runStateGen (generateBoard True word) <$> initStdGen
            (!hard_b,!hard_m, !hard_p) <- flip runStateGen_ (generateBoard False word) <$> pure g'
            modifyMVar isDone (\_ -> pure (True, ()))
            putMVar boardSyn ((b,m,p), (hard_b,hard_m,hard_p))
          void $ forkIO $ do
            -- After half a minute kill the generation
            threadDelay (40*1000*1000)
            done <- readMVar isDone
            when (not done) $ do
              putStrLn "Retrying..."
              killThread genTid
              tryGenBoard
    tryGenBoard
    ((board, nMoves, pick), (hard_board, hard_nMoves, hard_pick)) <- takeMVar boardSyn
    putStrLn $ "board-id:" ++ boardId board
    putStrLn $ "Constructing the solution took " ++ show nMoves ++ " moves"
    putStrLn $ "Solution pos " ++ show pick

    putStrLn $ "hard-board-id:" ++ boardId hard_board
    putStrLn $ "Constructing the hard board took " ++ show hard_nMoves ++ " moves"
    putStrLn $ "Hard solution pos " ++ show hard_pick
    --
    -- loop board
    -- loop (boardFromRows sampleBoard)

  where
  loop game = do
    print game
    inp <- getLine
    case readMaybe @(Int, Direction) inp of
      Nothing -> loop game
      Just (ix, dir) -> loop $ fromMaybe game $ move ix dir game

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
