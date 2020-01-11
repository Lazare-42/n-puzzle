module Heuristics where

import Parser 
import Debug.Trace

tmp :: Int -> Int
tmp x = trace (show x) x

columnForOneNumber :: Int -> Int -> Int
columnForOneNumber num gridSize = (num - 1) `quot` gridSize

rowForOneNumber :: Int -> Int -> Int
rowForOneNumber num gridSize = (num - 1) `mod` (gridSize)

manhattanPointValue :: Int -> Int -> Int -> Int
manhattanPointValue index gridSize 6 = abs(tmp ((rowForOneNumber index gridSize) - (rowForOneNumber 6 gridSize))) + abs(tmp((columnForOneNumber index gridSize) - (columnForOneNumber 6 gridSize)))
manhattanPointValue index gridSize point = abs((rowForOneNumber index gridSize) - (rowForOneNumber point gridSize)) + abs((columnForOneNumber index gridSize) - (columnForOneNumber point gridSize))

getManhattan :: Int -> Int -> [Int] -> Int
getManhattan index gridSize (0:xs) = 0 + getManhattan (index + 1) gridSize xs
getManhattan index gridSize (x:xs) = (manhattanPointValue index gridSize x) + getManhattan (index + 1) gridSize xs
getManhattan _ _ [] = 0

manhattan :: Map -> Int
manhattan map = getManhattan 1 (size map) (board map)
