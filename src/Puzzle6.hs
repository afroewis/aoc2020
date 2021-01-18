module Puzzle6 (puzzle6) where

import Paths_aoc2020
import Data.List.Split (splitOn)
import Data.List (nub, intersect)

puzzle6 :: IO()
puzzle6 = do
       filePath <- getDataFileName "data/puzzle6-input.txt"
       contents <-  readFile filePath
       let groups = splitOn "\n\n" contents
       let answersPerGroup = map lines groups

       -- Part 1
       let uniqueAnswersPerGroup = map (nub . concat) answersPerGroup
       let score = foldl (\acc x -> acc + length x) 0 uniqueAnswersPerGroup
       putStrLn $ "Number of answers: " ++ show score

       -- Part 2
       let sharedAnswers = map (nub . foldl1 intersect) answersPerGroup
       putStrLn $ "Number of shared answers: " ++ show (foldl (\acc x -> acc + length x) 0 sharedAnswers)

