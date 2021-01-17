module Puzzle5 (puzzle5) where

import Paths_aoc2020

data Seat = Seat {row :: Int, column :: Int} deriving (Show)

parseBoardingPass :: String -> Seat
parseBoardingPass str = Seat row column
    where rowBytes = take 7 str
          columnBytes = drop 7 str
          row = getRow rowBytes 0 127
          column = getColumn columnBytes 0 7

getRow :: String -> Int -> Int -> Int
getRow [] min _ = min
getRow (x:xs) min max = getRow xs nextMin nextMax
    where useUpperHalf = x == 'B'
          nextMin = if useUpperHalf then min + 1 + (max-min) `div` 2 else min
          nextMax = if useUpperHalf then max else min + (max-min) `div` 2

getColumn :: String -> Int -> Int -> Int
getColumn [] min _ = min
getColumn (x:xs) min max = getColumn xs nextMin nextMax
    where useUpperHalf = x == 'R'
          nextMin = if useUpperHalf then min + 1 + (max-min) `div` 2 else min
          nextMax = if useUpperHalf then max else min + (max-min) `div` 2

puzzle5 :: IO()
puzzle5 = do
       filePath <- getDataFileName "data/puzzle5-input.txt"
       contents <-  readFile filePath
       let boardingPasses =  lines contents

       let seats = map parseBoardingPass boardingPasses

       let seatIds =  [row x * 8 + column x | x <- seats]
       putStrLn $ "Max seat ID: " ++ show (maximum seatIds)


--       TO DO: Write unit tests with these test cases
--       putStrLn $ "Row FBFBBFF: " ++ show (getRow "FBFBBFF" 0 127) ++ " Column RLR: " ++ show (getColumn "RLR" 0 7)
--       putStrLn $ "Row BFFFBBF: " ++ show (getRow "BFFFBBF" 0 127) ++ " Column RRR: " ++ show (getColumn "RRR" 0 7)
--       putStrLn $ "Row FFFBBBF: " ++ show (getRow "FFFBBBF" 0 127) ++ " Column RRR: " ++ show (getColumn "RRR" 0 7)
--       putStrLn $ "Row BBFFBBF: " ++ show (getRow "BBFFBBF" 0 127) ++ " Column RLL: " ++ show (getColumn "RLL" 0 7)