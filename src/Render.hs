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
    heightIndex = map (\i -> if i < 10 then show i ++ " " else show i) [0 .. height - 1]
    widthIndex = "  " ++ concatMap (\i -> if i < 10 then show i ++ " " else show i) [0 .. width - 1]

renderBlock :: Block -> String
renderBlock Edge = ""
renderBlock (Hide _) = "⬛"
renderBlock (Show Bomb) = "💣"
renderBlock (Show Explode) = "💥"
renderBlock (Show Empty) = "⬜"
renderBlock (Show (Number n)) = renderNumber n

renderNumber :: Number -> String
renderNumber One = " 1"
renderNumber Two = " 2"
renderNumber Three = " 3"
renderNumber Four = " 4"
renderNumber Five = " 5"
renderNumber Six = " 6"
renderNumber Seven = " 7"
renderNumber Eight = " 8"
