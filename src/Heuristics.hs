module Heuristics where

import Parser 
import Debug.Trace

tmp :: Int -> Int
tmp x = trace (show x) x

columnForOneNumber :: Int -> Int -> Int
columnForOneNumber num gridSize = (num - 1) `mod` gridSize

rowForOneNumber :: Int -> Int -> Int
rowForOneNumber num gridSize = (num - 1) `quot` (gridSize)

manhattanPointValue :: Point -> Int -> Int -> Int
manhattanPointValue tile gridSize tileValue = abs((row tile) - (rowForOneNumber tileValue gridSize)) + abs((col tile) - (columnForOneNumber tileValue gridSize))

getManhattan :: Point -> Int -> [[Int]] -> Int
getManhattan point gridSize ((0:xs):ys) = 0 + getManhattan (Point (row point) (col point + 1)) gridSize (xs:ys)
getManhattan point gridSize ((x:xs):ys) = (manhattanPointValue point gridSize x) + getManhattan (Point (row point) (col point + 1)) gridSize (xs:ys)
getManhattan point gridSize [] = 0
getManhattan point gridSize ([]:ys) = getManhattan (Point (row point + 1) 0) gridSize ys

manhattan :: Map -> Int
manhattan map = getManhattan (Point 0 0) (size map) (board map)
