module Puzzle4
  ( puzzle4
  ) where

import Paths_aoc2020
import Data.List.Split
import Data.Maybe
import Text.Parsec
import Text.Parsec.String

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
data Height = Height {height :: Int, unit :: String} deriving (Show)
data Passport = Passport {byr :: Int, iyr :: Int, year :: Int} deriving (Show)

-- Parsing with Parsec
int :: Parser Int
int = read <$> many1 digit

heightParser :: Parser Height
heightParser = do height <- int
                  unit <- many1 Text.Parsec.letter
                  return (Height height unit)

hairColorParser :: Parser String
hairColorParser = do _ <- char '#'
                     color <- many1 hexDigit
                     return color

passportIdParser :: Parser String
passportIdParser = do
                     id <- many1 digit
                     return id

createPair :: String -> (String, String)
createPair str = (key, value)
    where values = splitOn ":" str
          key = head values
          value = last values

validateHeight :: String -> Bool
validateHeight hgtStr = case runParser heightParser () "" hgtStr of
            Left _ -> False
            Right h -> if unit h == "cm"
                         then height h >= 150 && height h <= 193
                       else
                       (unit h == "in") && (height h >= 59 && height h <= 76)

validateHairColor :: String -> Bool
validateHairColor hclStr = case runParser hairColorParser () "" hclStr of
            Left _ -> False
            Right hcl -> length hcl == 6

validatePassportId :: String -> Bool
validatePassportId pidStr = case runParser passportIdParser () "" pidStr of
            Left _ -> False
            Right pid -> length pid == 9

isValidPassport_1 :: [(String, String)] -> Bool
isValidPassport_1 lst = listLength == 8 || listLength == 7 && isNothing countryId
    where listLength = length lst
          countryId = lookup "cid" lst

isValidPassport_2 :: [(String, String)] -> Bool
isValidPassport_2 lst = byrValid && iyrValid && eyrValid && hgtValid && eclValid && hclValid && pidValid
    where byrValid = case lookup "byr" lst of
                        Nothing -> False
                        Just val -> read val >= 1920 && read val <= 2002
          iyrValid = case lookup "iyr" lst of
                        Nothing -> False
                        Just val -> read val >= 2010 && read val <= 2020
          eyrValid = case lookup "eyr" lst of
                        Nothing -> False
                        Just val -> read val >= 2020 && read val <= 2030
          hgtValid = maybe False validateHeight (lookup "hgt" lst)
          hclValid = maybe False validateHairColor (lookup "hcl" lst)
          eclValid = case lookup "ecl" lst of
                        Nothing -> False
                        Just val -> val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
          pidValid = maybe False validatePassportId (lookup "pid" lst)

puzzle4 :: IO()
puzzle4 = do
       filePath <- getDataFileName "data/puzzle4-input.txt"
       contents <-  readFile filePath

       let texts =  splitOn "\n\n" contents
       let stringPairs = map (endByOneOf " \n") texts

       let pairs = (map . map) createPair stringPairs
       putStrLn $ "Total passports: " ++ show (length pairs)

       let validPassports = [p | p <- pairs, isValidPassport_1 p]
       putStrLn $ "Total valid passports (Part 1): " ++ show (length validPassports)

       let validPassports2 = [p | p <- pairs, isValidPassport_2 p]
       putStrLn $ "Total valid passports (Part 2)): " ++ show (length validPassports2)
