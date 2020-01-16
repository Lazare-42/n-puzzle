module Heuristics where

import Structure

columnForOneNumber :: Int -> Int -> Int
columnForOneNumber num gridSize = (num - 1) `mod` gridSize

rowForOneNumber :: Int -> Int -> Int
rowForOneNumber num gridSize = (num - 1) `quot` (gridSize)

manhattanPointValue :: Point -> Int -> Int -> Integer
manhattanPointValue tile gridSize tileValue = toInteger (abs((row tile) - (rowForOneNumber tileValue gridSize)) + abs((col tile) - (columnForOneNumber tileValue gridSize)))

getManhattan :: Integer -> [Tile] -> Integer
getManhattan gridSize (x:xs) = case value x of
                                 0 -> recurse
                                 val -> recurse + (manhattanPointValue (coordinates x) (fromInteger gridSize) val)
  where recurse = getManhattan gridSize xs
getManhattan _ [] = 0
