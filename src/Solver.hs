module Solver where
import Structure
import Data.List


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
moveZero board heuristic newZeroP = Board (size board) newscore newZeroP newBoard (Just board)
                            where newBoard = swapTwoTilesOnMap zeroP newZeroP (tiles board)
                                  zeroP = zeroPos board
                                  newscore =  heuristic (size board) newBoard


getAdjacentRow :: Point -> (Int -> Int -> Int) -> Point
getAdjacentRow p f = Point (f 1 (row p)) (col p)


getAdjacentCol :: Point -> (Int -> Int -> Int) -> Point
getAdjacentCol p f = Point (row p) (f 1 (col p))


isFinalBoard :: Board -> Bool
isFinalBoard board = case score board of
                       0 -> True
                       _ -> False


applyHeuristicScore :: Int -> Board -> Board
applyHeuristicScore statenum board = result
  where result = (Board (size board) (newscore) (zeroPos board) (tiles board) (parent board))
        newscore = (statenum * score board)


applyHeuristicScoreOnList :: Int -> [Board] -> [Board]
applyHeuristicScoreOnList statenum l = result
  where result = tmp
        tmp = fmap (applyHeuristicScore statenum) l


accumulateMoveBoard :: GameState -> (Int -> [Tile] -> Int) -> String
accumulateMoveBoard state heuristic = case isFinalBoard (head (boardList state))  of
                                                True -> show state
                                                False -> accumulateMoveBoard tmp (heuristic)
                                                  where tmp = moveBoardOnce state heuristic


sortTwoBoards :: Board -> Board -> Ordering
sortTwoBoards fstBoard sndBoard = compare ((score fstBoard)) ((score sndBoard))


moveBoardOnce :: GameState-> (Int -> [Tile] -> Int) -> GameState
moveBoardOnce (GameState statenum (x:xs)) heuristic = GameState (statenum + 1) (sortBoards $ newboards)
  where sortBoards = sortBy sortTwoBoards 
        newboards = applyHeuristicScoreOnList statenum $ map (moveZero x heuristic) moves ++ xs
        moves = (possibleMoves (size x) (zeroPos x))
