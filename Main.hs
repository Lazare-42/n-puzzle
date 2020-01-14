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
                              let (size, list) = getSize $ stripComments $ map words (lines file)
                              return (show (Map size (findZeroPos 0 0 list) list))
                                  {-zeroPos = findZeroPos 0 0 map
                              case zeroPos of
                                Just x -> return (show ((Map size x map)))
                                Nothing -> return "no 0 in map."-}

main :: IO ()
main = do 
        args <- getArgs
        result <- launchExecution $ getArguments args
        putStrLn result
