module Puzzle1
  ( puzzle1
  ) where

import Paths_aoc2020
import Data.List

puzzle1 :: IO()
puzzle1 = do
       filePath <- getDataFileName "data/puzzle1-input.txt"
       contents <- readFile filePath
       let ints = map readInt . words $ contents

       print [(x,y, x*y) | (x:ys) <- tails ints, y <- ys, x+y == 2020]

readInt :: String -> Int
readInt = read
