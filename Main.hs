{-# LANGUAGE OverloadedRecordDot, RecordWildCards, LambdaCase, TypeApplications, ViewPatterns #-}

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

generateBoard :: String {- N letter word -} -> StdGen -> (Board, Int)
generateBoard word g = (Board{size, tiles}, nMoves) where
  size = length word
  nTilesFFR {- number of tiles in the first four rows -}
    = size*(size-1)
  nTiles {- total number of tiles -}
    = size*size

  (g', holesIxs) = List.mapAccumL (\acc _i -> swap $ uniformR (0, nTilesFFR - 1) acc) g [0..size-1]
  holes = IM.fromList $ map (,Empty) holesIxs
  (g'', IM.fromList -> fullLetters) = List.mapAccumL (\acc i -> swap $ first ((i,) . With) $ uniformR ('A', 'Z') acc) g' [0..nTilesFFR-1]
  lastRow = IM.fromList $ zip [nTilesFFR..size*size-1] (map With word)

  solutionBoard = holes `IM.union` lastRow `IM.union` fullLetters {- union is left biased, and no holes in the last row -}

  (initialBoard, nMoves) = loop Board{size, tiles=solutionBoard} 0 g'' where
    loop b nm rg
      | lastRowIsEmpty b
      = (b.tiles, nm)
      | otherwise
      = let (ix,rg') = uniformR (0,size*size-1) rg
            (mov,rg'') = uniformR (minBound @Direction, maxBound @Direction) rg'
         in if ix >= size*(size-2) && ix < size*(size-1) {- is in fourth row -}
               && mov == D then loop b nm rg'' {- don't try to push a piece in the fourth row down -}
            else
              let (b',nm') = maybe (b,nm) (,nm+1) $ move ix mov b
               in loop b' nm' rg''

  tiles = initialBoard

-- idea: use a genetic algorithm to figure out a "better" solution to the puzzle.

sampleBoard = [
  [With 'F', With 'U', With 'U', With 'M', With 'F'],
  [With 'Y', With 'I', With 'M', With 'J', With 'Z'],
  [With 'E', With 'C', With 'X', With 'S', With 'F'],
  [With 'Q', With 'R', With 'W', With 'D', With 'M'],
  [Empty, Empty, Empty, Empty, Empty]
              ]

main = do
  (board, nMoves) <- initStdGen >>= pure . generateBoard "STEAM"
  putStrLn $ "Solvable in number of moves: " ++ show nMoves
  putStrLn $ "Simple Board: " ++ boardId board
  loop board
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

lastRowIsEmpty :: Board -> Bool
lastRowIsEmpty Board{size, tiles} =
  all (==Empty) $ map (tiles IM.!) [size*(size-1)..size*size-1]

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
