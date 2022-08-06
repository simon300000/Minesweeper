module Render where

import Type (Block (..), Number (..), Space (..))

render :: [[Block]] -> String
render w = unlines board
  where
    raw = map tail (tail w)
    lines = map (concatMap renderBlock) raw
    board = widthIndex : zipWith (++) heightIndex lines
    height = length w - 2
    width = length (head w) - 2
    heightIndex = map show [0 .. height - 1]
    widthIndex = concatMap ((" " ++) . show) [0 .. width - 1]

renderBlock :: Block -> String
renderBlock Edge = ""
renderBlock (Hide _) = "⬛"
renderBlock (Show Bomb) = "💣"
renderBlock (Show Explode) = "💥"
renderBlock (Show Empty) = "⬜"
renderBlock (Show (Number n)) = renderNumber n

renderNumber :: Number -> String
renderNumber One = "1⃣️"
renderNumber Two = "2⃣️"
renderNumber Three = "3⃣️"
renderNumber Four = "4⃣️"
renderNumber Five = "5⃣️"
renderNumber Six = "6⃣️"
renderNumber Seven = "7⃣️"
renderNumber Eight = "8⃣️"
