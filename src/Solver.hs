module Solver where
import Structure
import Debug.Trace

changePositionofTile :: Point -> Point -> Tile -> Tile
changePositionofTile oldPosZero newPosZero recipient = case newPosZero == coordinates recipient of
                                         True -> Tile (value recipient) oldPosZero
                                         False -> recipient

changePositionofTileByValue :: Int -> Point -> Tile -> Tile
changePositionofTileByValue searchVal newPos recipient = case searchVal == value recipient of
                                         True -> Tile (value recipient) newPos
                                         False -> recipient


swapTwoTilesOnMap :: Point -> Point -> [Tile] -> [Tile]
swapTwoTilesOnMap oldPosZero newPosZero board = map (changePositionofTileByValue 0 newPosZero) $ map (changePositionofTile oldPosZero newPosZero) board

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

moveZero :: Board -> (Int -> [Tile] -> Int) -> Point -> Board
moveZero board heuristic newZeroP = Board (size board) (heuristic (size board) newBoard) newZeroP newBoard
                            where newBoard = swapTwoTilesOnMap zeroP newZeroP (tiles board)
                                  zeroP = zeroPos board

getAdjacentRow :: Point -> (Int -> Int -> Int) -> Point
getAdjacentRow p f = trace ((show p) ++ (show (Point (f (row p) 1) (col p)))) (Point (f (row p) 1) (col p))

getAdjacentCol :: Point -> (Int -> Int -> Int) -> Point
getAdjacentCol p f = Point (row p) (f (col p) 1)

moveBoardOnce :: Board -> (Int -> [Tile] -> Int) -> [Board]
moveBoardOnce board heuristic = map (moveZero board heuristic) (possibleMoves (size board) (zeroPos board))
