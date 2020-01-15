module Heuristics where

import Structure

columnForOneNumber :: Int -> Int -> Int
columnForOneNumber num gridSize = (num - 1) `mod` gridSize

rowForOneNumber :: Int -> Int -> Int
rowForOneNumber num gridSize = (num - 1) `quot` (gridSize)

manhattanPointValue :: Point -> Int -> Int -> Int
manhattanPointValue tile gridSize tileValue = abs((row tile) - (rowForOneNumber tileValue gridSize)) + abs((col tile) - (columnForOneNumber tileValue gridSize))

getManhattan :: Int -> [Tile] -> Int
getManhattan gridSize (x:xs) = case value x of
                                 0 -> recurse
                                 val -> (manhattanPointValue (coordinates x) gridSize val) + recurse
  where recurse = getManhattan gridSize xs
getManhattan _ [] = 0
