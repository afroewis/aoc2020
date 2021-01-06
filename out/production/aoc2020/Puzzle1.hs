module Puzzle1
  ( puzzle1
  ) where

import Paths_aoc2020
import Data.List

puzzle1 :: IO()
puzzle1 = do
       filePath <- getDataFileName "data/puzzle1-input.txt"
       contents <- readFile filePath
       let ints = map read' . words $ contents

       print [(x,y, x*y) | (x:ys) <- tails ints, y <- ys, x+y == 2020]
       print [(x,y,z, x*y*z) | (x:ys) <- tails ints, y <- ys, z <- ys, x+y+z == 2020]

read' :: String -> Int
read' = read
