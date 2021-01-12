module Puzzle4
  ( puzzle4
  ) where

import Paths_aoc2020
import Data.List.Split
import Data.Maybe

-- Available fields
------------------------
-- Birth Year (byr)
-- Issue Year (iyr)
-- Expiration Year (eyr)
-- Height (hgt)
-- Hair Color (hcl)
-- Eye Color (ecl)
-- Passport ID (pid)
-- Country ID (cid)
-------------------------

createPair :: String -> (String, String)
createPair str = (key, value)
    where values = splitOn ":" str
          key = head values
          value = last values

isValidPassport :: [(String, String)] -> Bool
isValidPassport lst = listLength == 8 || listLength == 7 && isNothing countryId
    where listLength = length lst
          countryId = lookup "cid" lst

puzzle4 :: IO()
puzzle4 = do
       filePath <- getDataFileName "data/puzzle4-input.txt"
       contents <-  readFile filePath

       let texts =  splitOn "\n\n" contents
       let stringPairs = map (endByOneOf " \n") texts

       let pairs = (map . map) createPair stringPairs
       putStrLn $ "Total passports: " ++ show (length pairs)

       let validPassports = [p | p <- pairs, isValidPassport p]
       putStrLn $ "Total valid passports: " ++ show (length validPassports)