module MW where

import Type (Block (..), Space (..))

validMove :: Block -> Bool
validMove Edge = False
validMove (Show _) = False
validMove _ = True

isBomb :: Block -> Bool
isBomb (Hide Bomb) = True
isBomb _ = False

isHideBomb :: Block -> Bool
isHideBomb (Hide Bomb) = True
isHideBomb (Hide _) = False
isHideBomb _ = True

showBomb :: [[Block]] -> (Int, Int) -> [[Block]]
showBomb board (x, y) = [[if (x, y) == (x', y') then Show Explode else replace (board !! y' !! x') | x' <- [0 .. width - 1]] | y' <- [0 .. height - 1]]
  where
    (height, width) = (length board, length (head board))
    replace :: Block -> Block
    replace (Hide Bomb) = Show Bomb
    replace x = x

showWhatever :: Block -> Block
showWhatever (Hide x) = Show x
showWhatever x = x

showX :: [Block] -> Int -> [Block]
showX (x : xs) 0 = showWhatever x : xs
showX (x : xs) n = x : showX xs (n - 1)
showX [] _ = []

showXY :: [[Block]] -> (Int, Int) -> [[Block]]
showXY (l : ls) (x, 0) = showX l x : ls
showXY (l : ls) (x, y) = l : showXY ls (x, y - 1)
showXY [] _ = []

s :: [[Block]] -> [(Int, Int)] -> [[Block]]
s board [] = board
s board ((x, y) : hs) = s newBoard newHs
  where
    (height, width) = (length board, length (head board))
    this = board !! y !! x
    isEmpty = this == Hide Empty
    newBoard = showXY board (x, y)
    newHs = if isEmpty then [(x, y) | x <- [x - 1, x, x + 1], y <- [y - 1, y, y + 1], validMove (newBoard !! y !! x), (x, y) `notElem` hs] ++ hs else hs

play :: [[Block]] -> (Int, Int) -> ([[Block]], Bool)
play board (x, y) = do
  (if valid then result else (board, True))
  where
    this = board !! y !! x
    valid = validMove this
    bomb = isBomb this
    newBoard = s board [(x, y)]
    finished = all (all isHideBomb) newBoard
    newNewBoard = if finished then map (map showWhatever) newBoard else newBoard
    result = if bomb then (showBomb board (x, y), False) else (newNewBoard, not finished)
