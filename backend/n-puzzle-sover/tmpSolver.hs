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
possibleMoves boardSize zeroPos =  (validMoves boardSize (up ++ right ++ down ++ left))
  where up = [getAdjacentRow zeroPos (flip (+))]
        down = [getAdjacentRow zeroPos subtract]
        right = [getAdjacentCol zeroPos (flip (+))]
        left = [getAdjacentCol zeroPos subtract]

moveZero :: Board -> (Int -> [Tile] -> Int) -> Point -> Board
moveZero board heuristic newZeroP = Board (size board) (heuristic (size board) newBoard) newZeroP newBoard
                            where newBoard = swapTwoTilesOnMap zeroP newZeroP (tiles board)
                                  zeroP = zeroPos board

getAdjacentRow :: Point -> (Int -> Int -> Int) -> Point
getAdjacentRow p f = Point (f 1 (row p)) (col p)

getAdjacentCol :: Point -> (Int -> Int -> Int) -> Point
getAdjacentCol p f = Point (row p) (f 1 (col p))

moveBoardOnce :: Board -> (Int -> [Tile] -> Int) -> [Board]
moveBoardOnce board heuristic = map (moveZero board heuristic) (possibleMoves (size board) (zeroPos board))

compareTwoBoards :: Board -> Board -> Ordering
compareTwoBoards fst snd 
  | (score fst) <= (score snd) = LT
  | otherwise = GT


isTerminalState :: GameState -> Bool
isTerminalState state = case (score (self (solutions (state)))) of
                          0 -> True
                          _ -> False

addNewBoardsToList :: Tree -> Tree
addNewBoardsToList = sortBy compareTwoBoards [newMoves]
