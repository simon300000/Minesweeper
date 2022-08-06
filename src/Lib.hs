module Lib where

import Control.Monad (when)
import Generator (gen, genEmpty)
import MW (play)
import Render (render)
import Type (Block)

readInteger :: IO Int
readInteger = do read <$> getLine

checkBound :: Int -> Int -> Bool
checkBound x y = x >= 0 && x < y

checkBounds :: (Int, Int) -> (Int, Int) -> Bool
checkBounds (x, y) (x', y') = checkBound x x' && checkBound y y'

inputSize :: IO (Int, Int)
inputSize = do
  putStrLn "Enter the width of the board:"
  width <- readInteger
  putStrLn "Enter the height of the board:"
  height <- readInteger
  if checkBounds (0, 0) (width, height)
    then return (width, height)
    else do
      putStrLn ("invalid board size, " ++ show (width, height))
      inputSize

inputMines :: Int -> IO Int
inputMines max = do
  putStrLn "Enter number of mines:"
  x <- readInteger
  if checkBound x max
    then return x
    else do
      putStrLn ("Invalid number of mines, " ++ show x)
      inputMines max

inputClick :: (Int, Int) -> IO (Int, Int)
inputClick (width, height) = do
  putStrLn "Enter x:"
  x <- readInteger
  putStrLn "Enter y:"
  y <- readInteger
  if checkBounds (x, y) (width, height)
    then return (x, y)
    else do
      putStrLn ("Invalid coordinates, " ++ show (x, y))
      inputClick (width, height)

click :: ([[Block]], (Int, Int)) -> (Int, Int) -> IO ()
click (board, (width, height)) (x, y) = do
  putStr (render newBoard)
  when good (inputClick (width, height) >>= click (newBoard, (width, height)))
  where
    (newBoard, good) = play board (x + 1, y + 1)

start :: IO ()
start = do
  putStrLn "============================================================"
  putStrLn "======================= Minesweeper ========================"
  putStrLn "============================================================"
  (width, height) <- inputSize
  mines <- inputMines (width * height)
  putStr (render (genEmpty (width, height)))
  (x, y) <- inputClick (width, height)
  board <- gen (width, height) (x, y) mines
  click (board, (width, height)) (x, y)
