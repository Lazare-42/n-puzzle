module Parser where

import Data.List
import Control.Monad

data Map = Map {
          size :: Int,
          zeroPos :: Point,
          board :: [[Int]]
               }
          deriving (Show, Eq)

data Point = Point {
row :: Int,
col :: Int
                   }
          deriving (Show, Eq)
newtype Gridsize s = Int s


data Arguments = Arguments {
        filepath :: String
                           }
          deriving (Show, Eq)

createMapList :: Int -> [Int] -> [[Int]]
createMapList size (x:xs) = [take size (x:xs)] `mplus` createMapList size (drop size (x:xs))
createMapList _ [] = []

readI :: String -> Int
readI s = read s

isBeginComment :: String -> Bool
isBeginComment ('#':xs) = True
isBeginComment _ = False

getOneLine :: [String] -> [Int]
getOneLine (x:xs) = case isBeginComment x of
                      True -> []
                      False -> [readI x] ++ getOneLine xs
getOneLine [] = []

getMapList :: [String] -> [Int]
getMapList (x:xs)  = (getOneLine $ words x) `mplus` getMapList xs
getMapList [] = []

getOptionValue :: String -> [String] -> Maybe String
getOptionValue option (x:xs) = case option == x of
                                True -> case xs of
                                  [] -> Nothing
                                  (y:ys) -> Just y
                                False -> getOptionValue option xs
getOptionValue option [] = Nothing

findZeroPos :: Int -> Int -> [[Int]] -> Maybe Point
findZeroPos row col ((0:xs):ys) = Just (Point row col)
findZeroPos row col ((x:xs):ys) = findZeroPos row (col + 1) (xs:ys)
findZeroPos row col ([]:ys) = findZeroPos (row + 1) 0 ys
findZeroPos _ _ [] = Nothing

getArguments :: [String] -> Maybe Arguments
getArguments args = Arguments `liftM` getOptionValue "file" args
