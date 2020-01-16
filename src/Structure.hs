module Structure where
import Data.List
import Debug.Trace

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

-- data Tree = Root Tree
--           | Leaf 
--             {
--             parent :: Maybe Tree,
--             self   :: Board,
--             child  :: [Tree]
--             }

data GameState = GameState {
numberOfTurns :: Int,
boardList     :: [Board]
                           }
instance Show GameState where
  show state = show (head $ boardList state) ++ ("\nFound a solution after: " ++ (show (numberOfTurns state)) ++ ".turn[s].\nThe total number of states examined is: " ++ show (length(boardList state)))

data Board = Board {
size    :: Int,
score   :: Int,
zeroPos :: Point,
tiles :: [Tile],
parent :: Maybe Board 
                   }
instance Show Board where
  show x = showBoard 0 x

compareTwoTiles :: Tile -> Tile  -> Ordering
compareTwoTiles fstTile sndTile
  | (row (coordinates fstTile)) < (row (coordinates sndTile)) = LT
  | (row (coordinates fstTile)) == (row (coordinates sndTile)) && (col (coordinates fstTile)) < (col (coordinates sndTile)) = LT
  | otherwise = GT


showBoard :: Int -> Board -> String
showBoard 0 board = case (parent board) of
                      Just m -> (showBoard 0 m) ++ (showBoard 0 (Board (size board) (score board) (zeroPos board) (tiles board) Nothing))
                      Nothing -> "\nThe value of this board is: " ++ show (score board) ++ ".\n" ++
                                  "The free tile is located at: " ++ (show (zeroPos board)) ++ ".\n" ++
                                    (showBoard 1 $ Board (size board) (score board) (zeroPos board) (sortBy compareTwoTiles (tiles board)) (parent board))
showBoard iter (Board size score zeroPos (x:xs) parent) = case (iter `mod` size == 0) of
                                                     True -> show x ++ "\n" ++ recurse
                                                     False -> show x ++ " " ++ recurse
  where recurse = showBoard (iter + 1) (Board size score zeroPos xs parent)
showBoard _ (Board _ _ _ [] _) = ""

data Arguments = Arguments {
        filepath :: String
                           }
          deriving (Show, Eq)
