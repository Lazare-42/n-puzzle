module Parser where

import Data.List
import Control.Monad

data Map = Map {
          size :: Int,
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
createMapList size (x:xs) = [take size (x:xs)] ++ createMapList size (drop size (x:xs))
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

getMap :: [String] -> [Int]
getMap (x:xs)  = (getOneLine $ words x) ++ getMap xs
getMap [] = []

getOptionValue :: String -> [String] -> Maybe String
getOptionValue option (x:xs) = case option == x of
                                True -> case xs of
                                  [] -> Nothing
                                  (y:ys) -> Just y
                                False -> getOptionValue option xs
getOptionValue option [] = Nothing

getArguments :: [String] -> Maybe Arguments
getArguments args = Arguments `liftM` getOptionValue "file" args
