module Structure where

data Point = Point {
row :: Int,
col :: Int
                   }
          deriving (Eq)

instance Show Point where
  show x = "(" ++ show (row x) ++ ", " ++ show (col x) ++ ")"
newtype Gridsize s = Int s

data Tile = Tile {
value :: Int,
coordinates :: Point
                 }
          deriving (Eq)

instance Show Tile where
  show (Tile value coordinates) = show value

data Board = Board {
size    :: Int,
score   :: Int,
zeroPos :: Point,
tiles :: [Tile]
                   }
instance Show Board where
  show x = showBoard 0 x

showBoard :: Int -> Board -> String
showBoard 0 board = "\nThe value of this board is: " ++ show (score board) ++ "\n" ++ "The free tile is located at: " ++ (show (zeroPos board)) ++ "\n" ++ showBoard 1 board
showBoard iter (Board size score zeroPos (x:xs)) = case (iter `mod` size == 0) of
                                                     True -> show x ++ "\n" ++ recurse
                                                     False -> show x ++ " " ++ recurse
  where recurse = showBoard (iter + 1) (Board size score zeroPos xs)
showBoard _ (Board _ _ _ []) = ""



data Arguments = Arguments {
        filepath :: String
                           }
          deriving (Show, Eq)


