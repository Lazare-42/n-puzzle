module Main where

import System.Environment
import Data.List
import Control.Monad
import Solver 
import Parser
import Heuristics

{-getMap :: IO String -> Maybe Map
getMap file = do
                    mapList <- getMapList $ lines $ file
                    let (size, list) = ((head x),(drop 1 x))
                    map <- (createMapList size list)
                    zeroPos <- findZeroPos 0 0 map
                    Just (Map size zeroPos map)-}



launchExecution :: Maybe Arguments -> IO String
launchExecution Nothing = return "Usage : file [filename]"
launchExecution (Just arg) = do
                              file <- readFile (filepath arg)
                              let x = getMapList $ lines $ file
                                  (size, list) = ((head x),(drop 1 x))
                                  map = (createMapList size list)
                                  zeroPos = findZeroPos 0 0 map
                              case zeroPos of
                                Just x -> return (show ((Map size x map)))
                                Nothing -> return "no 0 in map."

main :: IO ()
main = do 
        args <- getArgs
        result <- launchExecution $ getArguments args
        putStrLn result
