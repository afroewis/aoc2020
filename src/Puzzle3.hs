module Puzzle3
  ( puzzle3
  ) where

import Paths_aoc2020

replaceInput :: Int -> [String] -> [String]
replaceInput _ [] = []
replaceInput i [x] = [replaceCharAtIndex (normalizeX maxLength i) x]
  where maxLength = length x
replaceInput i (x:xs) = replaceInput i [x] ++ replaceInput (i + 3) xs

replaceCharAtIndex :: Int -> String -> String
replaceCharAtIndex x str = strHead ++ [replacement] ++ tail strAfter
  where (strHead, strAfter) = splitAt x str
        replacement = if str !! x == '.' then 'O' else 'X'

normalizeX :: Int -> Int -> Int
normalizeX maxLength currentLength
  | currentLength < maxLength = currentLength
  | otherwise = currentLength `mod` maxLength

puzzle3 :: IO()
puzzle3 = do
       filePath <- getDataFileName "data/puzzle3-input.txt"
       contents <-  readFile filePath
       let lines2 = lines contents
       let result = replaceInput 3 $ tail lines2

       putStrLn $ "Number of trees crossed: " ++ show (length [c | c <- concat result, c=='X'])
       mapM_ putStrLn $ take 5 result
       putStrLn "..."
