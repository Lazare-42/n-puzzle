module Solver where
import Structure

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
