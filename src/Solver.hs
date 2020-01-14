module Solver where
import Parser

{-import Parser
import Control.Monad

newtype Value = Int

newtype MoveNumber = Int

data MoveValue = (Value, MoveNumber)

data GameBoard = (Map, MoveValue)

data OnePath = OnePath [GameBoard]

data GameState = GameState {
numberOfBoards  :: Int,
pathList        :: OnePath
                           }

swapTwoTilesOnMap :: Point -> Point -> Map -> Map
swapTwoTilesOnMap (rowFirst, colFirst) (zeroRow, zeroCol) map = map !! rowFirst 

returnUpperVerticalMove = GameBoard -> [GameBoard]
returnUpperVerticalMove (map, movevalue) = case row $ zerooooPos map of
                                         0 -> []
                                         x -> 


returnPossibleMoves :: GameBoard -> [GameBoard]
returnPossibleMoves (map, movevalue) = case row $ zerooooPos map of
                                         0 -> []
                                         x -> 
-}

data Tile = Tile {
value :: Int,
coordinates :: Point
                 }

data Board = Board {
sizee    :: Int,
score   :: Int,
zerooooPos :: Point,
tiles :: [Tile]
                   }

changePositionofTile :: Point -> Tile -> Tile
changePositionofTile donor recipient = case donor == coordinates recipient of
                                         True -> Tile (value recipient) donor
                                         False -> recipient

swapTwoTilesOnMap :: Point -> Point -> [Tile] -> [Tile]
swapTwoTilesOnMap fst snd board = map (changePositionofTile snd) $ map (changePositionofTile fst) board

getAdjacentRow :: Point -> (Int -> Int -> Int) -> Point
getAdjacentRow p f = Point (f (row p) 1) (col p)

getAdjacentCol :: Point -> (Int -> Int -> Int) -> Point
getAdjacentCol p f = Point (row p) (f (col p) 1)

returnUpperMove :: Board -> [Board]
returnUpperMove board = case row $ zerooooPos board of
                        0 -> []
                        x -> [Board (sizee board) (score board) newZeroP newBoard]
                          where newBoard = swapTwoTilesOnMap (zerooooPos board) (getAdjacentRow (zerooooPos board) subtract) (tiles board)
                                newZeroP = zerooooPos board

returnLeftMove :: Board -> [Board]
returnLeftMove board = case col $ zerooooPos board of
                        0 -> []
                        x -> [Board (sizee board) (score board) newZeroP newBoard]
                          where newBoard = swapTwoTilesOnMap zeroP newZeroP (tiles board)
                                newZeroP = (getAdjacentCol zeroP subtract)
                                zeroP = zerooooPos board

{-returnBoardAfterMove :: (Int -> Int -> Int) -> Board -> [Board]
returnBoardAfterMove f board = case -}

