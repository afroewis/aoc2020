module Puzzle10 (puzzle10) where

import Paths_aoc2020
import Data.List
puzzle10 :: IO()
puzzle10 = do
     filePath <- getDataFileName "data/puzzle10-input.txt"
     contents <- readFile filePath
     let fileLines = lines contents
     let numbers = sort $ map read fileLines
     let res = findJoltageDifferences numbers []
     let groups = (group . sort) res
     let answer = (length (head groups), length (last groups))
     putStrLn $ "1 Jolt differences: " ++ show (fst answer) ++ ", 3 Jolt differences: " ++ show (snd answer) ++
                ", Answer: " ++ show (uncurry (*) answer)

findJoltageDifferences :: [Int] -> [Int] -> [Int]
findJoltageDifferences [] res     = res ++ [3]
findJoltageDifferences (x:xs) res = findJoltageDifferences xs res' where
    currJolts = sum res
    delta     = x - currJolts
    res'      = res ++ [delta]