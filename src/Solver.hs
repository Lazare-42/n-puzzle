module Solver where
import Parser

data Board = Board {
size    :: Int,
score   :: Int,
zeroPos :: Point,
tiles :: [Tile]
                   }
instance Show Board where
  show x = showBoard 0 x

showBoard :: Int -> Board -> String
showBoard 0 board = "The value of this board is: " ++ show (score board) ++ "\n" ++ "The free tile is located at: " ++ (show (zeroPos board)) ++ "\n" ++ showBoard 1 board
showBoard iter (Board size score zeroPos (x:xs)) = case (iter `mod` size == 0) of
                                                     True -> show x ++ "\n" ++ recurse
                                                     False -> show x ++ " " ++ recurse
  where recurse = showBoard (iter + 1) (Board size score zeroPos xs)
showBoard _ (Board _ _ _ []) = ""


changePositionofTile :: Point -> Tile -> Tile
changePositionofTile donor recipient = case donor == coordinates recipient of
                                         True -> Tile (value recipient) donor
                                         False -> recipient

swapTwoTilesOnMap :: Point -> Point -> [Tile] -> [Tile]
swapTwoTilesOnMap fst snd board = map (changePositionofTile snd) $ map (changePositionofTile fst) board

validMove :: Int -> Point -> Bool
validMove boardSize zeroPos = row zeroPos >= 0 && col zeroPos >= 0 && row zeroPos < boardSize && col zeroPos < boardSize

validMoves :: Int -> [Point] -> [Point]
validMoves boardSize zeroPosList = filter (validMove boardSize) zeroPosList

possibleMoves :: Int -> Point -> [Point]
possibleMoves boardSize zeroPos = validMoves boardSize (up ++ down ++ left ++ right)
  where up = [getAdjacentRow zeroPos subtract]
        down = [getAdjacentRow zeroPos (flip (+))]
        left = [getAdjacentCol zeroPos subtract]
        right = [getAdjacentCol zeroPos (flip (+))]

moveZero :: Board -> Point -> Board
moveZero board newZeroP = Board (size board) (score board) newZeroP newBoard
                            where newBoard = swapTwoTilesOnMap zeroP newZeroP (tiles board)
                                  zeroP = zeroPos board

getAdjacentRow :: Point -> (Int -> Int -> Int) -> Point
getAdjacentRow p f = Point (f (row p) 1) (col p)

getAdjacentCol :: Point -> (Int -> Int -> Int) -> Point
getAdjacentCol p f = Point (row p) (f (col p) 1)

moveBoardOnce :: Board -> [Board]
moveBoardOnce board = map (moveZero board) (possibleMoves (size board) (zeroPos board))
