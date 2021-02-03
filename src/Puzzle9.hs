module Puzzle9 (puzzle9) where

import Paths_aoc2020
import Data.Text.Read
import Data.Text (pack)
import Data.List (tails)

puzzle9 :: IO()
puzzle9 = do
     filePath <- getDataFileName "data/puzzle9-input.txt"
     contents <- readFile filePath
     let fileLines = lines contents
     let numbers = map readInt fileLines
     let numbers2 = findIllegalNumbers numbers []
     mapM_ print numbers2

     -- Create unit tests
     let arr = [35, 20, 15, 25, 47]
     print $ numberIsAllowed arr 40

     let arr2 = [20, 15, 25, 47, 40]
     print $ numberIsAllowed arr2 62

     let arr3 = [15, 25, 47, 40, 62]
     print $ numberIsAllowed arr3 55

     let arr4 = [25, 47, 40, 62, 55]
     print $ numberIsAllowed arr4 65

     let arr5 = [95, 102, 117, 150, 182]
     print $ numberIsAllowed arr5 127 -- Expected to be false


findIllegalNumbers :: [Int] -> [Int] -> [Int]
findIllegalNumbers numbers@(x:xs) results
  | length numbers < 26 = results
  | otherwise = results ++ findIllegalNumbers xs results'
  where
      preamble      = take 25 numbers
      results'      = [numbers !! 25 | not numberPasses]
      numberPasses  = numberIsAllowed preamble (numbers !! 25)

numberIsAllowed :: [Int] -> Int -> Bool
numberIsAllowed preamble num = not (null ([(x,y) | x <- preamble, y <- preamble, x+y == num]))

readInt :: String -> Int
readInt str = read str