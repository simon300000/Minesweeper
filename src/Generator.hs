module Generator where

import Data.List (genericReplicate)
import Type (Block (..), Space (..))

genEmpty :: (Integer, Integer) -> [[Block]]
genEmpty (x, y) = edge : genericReplicate y (Edge : genericReplicate x (Hide Empty) ++ [Edge]) ++ [edge]
  where
    edge = genericReplicate (x + 2) Edge

gen :: (Integer, Integer) -> (Integer, Integer) -> Integer -> [[Block]]
gen (x, y) noBomb bombs = edge : [] ++ [edge]
  where
    edge = genericReplicate (x + 2) Edge
