module Parser where

import Data.List
import Control.Monad

data Tile = Tile {
value :: Int,
coordinates :: Point
                 }
          deriving (Eq)

instance Show Tile where
  show (Tile value coordinates) = show value

data Point = Point {
row :: Int,
col :: Int
                   }
          deriving (Eq)

instance Show Point where
  show x = "(" ++ show (row x) ++ ", " ++ show (col x) ++ ")"
newtype Gridsize s = Int s


data Arguments = Arguments {
        filepath :: String
                           }
          deriving (Show, Eq)


stripComments :: [[String]] -> [[String]]
stripComments l =  filter (not . null) $ map (takeWhile isNotBeginComment) l

isNotBeginComment :: String -> Bool
isNotBeginComment ('#':xs) = False
isNotBeginComment _ = True

getSize :: [String] -> (Int, [Int])
getSize l = (read (head l), map read $ drop 1 l)

createMapList :: Int -> Int -> Int -> [Int] -> [Tile]
createMapList size r c (x:xs) = [Tile x (Point r c)] ++ rest
  where rest = case c == (size - 1) of
                 True -> createMapList size (r + 1) 0 (xs)
                 False -> createMapList size r (c + 1) (xs)
createMapList _ _ _ [] = []


getOptionValue :: String -> [String] -> Maybe String
getOptionValue option (x:xs) = case option == x of
                                True -> case xs of
                                        (y:ys) -> Just y
                                        _ -> Nothing
                                False -> getOptionValue option xs
getOptionValue option [] = Nothing

findZeroPos :: [Tile] -> Point
findZeroPos map = coordinates $ head $ filter (\x -> (value x) == 0) map

getArguments :: [String] -> Maybe Arguments
getArguments args = Arguments `liftM` getOptionValue "file" args
