module Main where

import Lib
{-import Graphics.Image

returnImagePosition :: (Int, Int) -> Int -> (Int, Int)
returnImagePosition (iter, totalImages) originSize = case 

createImages :: Image VU RGBA Double -> (Int, Int) -> IO ()
createImages img (iter, totalImages) = case iter == totalImages of
                                         True -> print "created Images"
                                         False -> writeImage "./test.jpg" $ crop (0,0) (1000,1000) img
                                           --createImages img (iter + 1, totalImages)


main :: IO ()
main = do
  logo <- readImageRGBA VU "./van_gogh.jpg"
  createImages logo (1, 9)-}
  --let incBy (fm, fn) = (rows logo * fm, cols logo * fn)
  --print()
  --print(incBy (6, 10))
--  writeImage "./test.png" $ canvasSize Wrap (incBy (6, 10)) logo
--  writeImage "./test2.png" $ translate (Fill 0) (incBy (2, 3)) $ canvasSize (Fill 0) (incBy (5, 7)) logo

main :: IO ()
main = startApp
