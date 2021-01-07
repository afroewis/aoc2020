module Puzzle2
  ( puzzle2
  ) where

import Text.Parsec
import Text.Parsec.String
import Paths_aoc2020

data PasswordCheck = PasswordCheck Int Int Char String deriving Show

isValid_Part1 :: PasswordCheck -> Bool
isValid_Part1 (PasswordCheck minOcc maxOcc c subject)
  | occ >= minOcc && occ <= maxOcc = True
  | otherwise = False
  where occ = length $ filter (==c) subject

isValid_Part2 :: PasswordCheck -> Bool
isValid_Part2 (PasswordCheck firstIndex secondIndex c subject)
  | firstMatch /= secondMatch = True
  | otherwise = False
  where firstMatch = subject !! (firstIndex - 1) == c
        secondMatch = subject !! (secondIndex - 1) == c

str :: Parser String
str = many1 $ noneOf ","

int :: Parser Int
int = read <$> many1 digit

parser :: Parser PasswordCheck
parser = do minOcc <- int
            _ <- oneOf "-"
            maxOcc <- int
            _ <- oneOf " "
            char <- letter
            _ <- string ": "
            password <- str
            return (PasswordCheck minOcc maxOcc char password)

puzzle2 :: IO ()
puzzle2 = do  filePath <- getDataFileName "data/puzzle2-input.txt"
              contents <- readFile filePath
              let fileLines = lines contents
              let checks = parsePasswordCheck <$> fileLines
              
              print $ length $ filter runRule1 checks
              print $ length $ filter runRule2 checks

parsePasswordCheck :: String -> Maybe PasswordCheck
parsePasswordCheck s = do
              let x = runParser parser () "" s
              case x of
                Left e  -> Nothing
                Right y ->  Just y

runRule1 :: Maybe PasswordCheck -> Bool
runRule1 = maybe False isValid_Part1

runRule2 :: Maybe PasswordCheck -> Bool
runRule2 = maybe False isValid_Part2