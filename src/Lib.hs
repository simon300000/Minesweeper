module Lib where

import Generator (genEmpty)
import Render (render)

readInteger :: IO Integer
readInteger = do read <$> getLine

start :: IO ()
start = do
  putStrLn "Minesweeper"
  putStrLn "Enter the width of the board:"
  width <- readInteger
  if width <= 0
    then do putStrLn $ "Invalid width <= 0, " ++ show width
    else do
      putStrLn "Enter the height of the board:"
      height <- readInteger
      if height <= 0
        then do putStrLn $ "Invalid height <= 0, " ++ show height
        else do
          putStrLn "Enter the number of mines:"
          mines <- readInteger
          if mines <= 0 || mines >= (height * width)
            then do putStrLn $ "invalid number of mines, " ++ show mines
            else do
              let board = genEmpty (width, height)
              putStr (render board)
