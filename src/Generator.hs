module Generator where

import Data.List (genericReplicate)
import Data.List.Split (chunksOf)
import GHC.OldList (genericIndex, genericSplitAt)
import System.Random.Shuffle (shuffleM)
import Type (Block (..), Space (..))

genEmpty :: (Int, Int) -> [[Block]]
genEmpty (x, y) = edge : genericReplicate y (Edge : genericReplicate x (Hide Empty) ++ [Edge]) ++ [edge]
  where
    edge = genericReplicate (x + 2) Edge

shuffle :: [Block] -> IO [Block]
shuffle list = do
  shuffled <- shuffleM end
  return (start : shuffled)
  where
    end = tail list
    start = head list

swap :: Int -> [a] -> [a]
swap 0 list = list
swap i list = head b : tail a ++ head a : tail b
  where
    (a, b) = genericSplitAt i list

replaceBlock :: Block -> Int -> Block
replaceBlock x 0 = x
replaceBlock (Hide Empty) w = Hide (Number (toEnum w))
replaceBlock x _ = x

isBomb :: Block -> Int
isBomb (Hide Bomb) = 1
isBomb _ = 0

putNumber :: [[Block]] -> (Int, Int) -> Block
putNumber board (x, y) = if this == Edge then this else replaceBlock this countBomb
  where
    this = board !! y !! x
    countBomb = sum [isBomb (board !! y !! x) | x <- [x - 1, x, x + 1], y <- [y - 1, y, y + 1]]

putNumbers :: [[Block]] -> [[Block]]
putNumbers board = [[putNumber board (x, y) | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
  where
    (height, width) = (length board, length (head board))

gen :: (Int, Int) -> (Int, Int) -> Int -> IO [[Block]]
gen (x, y) (x', y') bombs = do
  shuffled <- shuffle list
  let bombPlaced = swap (x * y' + x') shuffled
  let formated = map (\w -> Edge : w ++ [Edge]) (chunksOf x bombPlaced)
  return (putNumbers (edge : formated ++ [edge]))
  where
    edge = genericReplicate (x + 2) Edge
    space = x * y
    empty = space - bombs
    list = genericReplicate empty (Hide Empty) ++ genericReplicate bombs (Hide Bomb)
