module Main where

import System.Environment
import Data.List
import Control.Monad
import Solver 
import Parser
import Heuristics

launchExecution :: Maybe Arguments -> IO String
launchExecution Nothing = return "Usage : file [filename]"
launchExecution (Just arg) = do
                              file <- readFile (filepath arg)
                              let x = getMap $ lines $ file
                                  (size, list) = ((head x),(drop 1 x))
                              return (show (manhattan (Map size (list))))

main :: IO ()
main = do 
        args <- getArgs
        result <- launchExecution $ getArguments args
        putStrLn result
