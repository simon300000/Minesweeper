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
showBomb board (x, y) = fXY (map (map replace) board) (x, y) (const (Show Explode))
  where
    (height, width) = (length board, length (head board))
    replace :: Block -> Block
    replace (Hide Bomb) = Show Bomb
    replace x = x

showWhatever :: Block -> Block
showWhatever (Hide x) = Show x
showWhatever x = x

fX :: [Block] -> Int -> (Block -> Block) -> [Block]
fX (x : xs) 0 f = f x : xs
fX (x : xs) n f = x : fX xs (n - 1) f
fX [] _ _ = []

fXY :: [[Block]] -> (Int, Int) -> (Block -> Block) -> [[Block]]
fXY (l : ls) (x, 0) f = fX l x f : ls
fXY (l : ls) (x, y) f = l : fXY ls (x, y - 1) f
fXY [] _ _ = []

s :: [[Block]] -> [(Int, Int)] -> [[Block]]
s board [] = board
s board ((x, y) : hs) = if valid then s newBoard newHs else s board hs
  where
    (height, width) = (length board, length (head board))
    this = board !! y !! x
    valid = validMove this
    isEmpty = this == Hide Empty
    newBoard = fXY board (x, y) showWhatever
    newHs = if isEmpty then [(x, y) | x <- [x - 1, x, x + 1], y <- [y - 1, y, y + 1]] ++ hs else hs

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
