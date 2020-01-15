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
                              let initialVal = getManhattan size board
                              let zeroPos = findZeroPos board
                              return (concat $ map show (moveBoardOnce (Board size initialVal zeroPos board) getManhattan))

main :: IO ()
main = do 
        args <- getArgs
        result <- launchExecution $ getArguments args
        putStrLn result
