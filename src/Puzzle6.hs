module Puzzle6 (puzzle6) where

import Paths_aoc2020
import Data.List.Split (splitOn)
import Data.List (nub)

puzzle6 :: IO()
puzzle6 = do
       filePath <- getDataFileName "data/puzzle6-input.txt"
       contents <-  readFile filePath
       let groups = splitOn "\n\n" contents
       let answersPerGroup = map lines groups
       
       -- Part 1
       let uniqueAnswersPerGroup = map (nub . concat) answersPerGroup
       let answer = foldl (\acc x -> acc + length x) 0 uniqueAnswersPerGroup

       putStrLn $ "Number of answers: " ++ show answer
