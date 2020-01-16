module Main where

import System.Environment
import Data.List
import Control.Monad
import Solver 
import Parser
import Heuristics
import Structure

launchExecution :: Maybe Arguments -> IO String
launchExecution Nothing = return "Usage : file [filename]"
launchExecution (Just arg) = do
                              file <- readFile (filepath arg)
                              let (size, list) = getSize $ concat $ stripComments $ map words (lines file)
                              let board = createMapList size 0 0 list
                              let initialVal = getManhattan (toInteger size) board
                              let zeroPos = findZeroPos board
                              return ((accumulateMoveBoard(GameState 1 [(Board (toInteger size) initialVal zeroPos board Nothing)])) getManhattan)

main :: IO ()
main = do 
        args <- getArgs
        result <- launchExecution $ getArguments args
        putStrLn result
