module Puzzle2
  ( puzzle2
  ) where

import Text.Parsec
import Text.Parsec.String
import Paths_aoc2020

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

data PasswordCheck = PasswordCheck Int Int Char String deriving Show

rule1 :: PasswordCheck -> Bool
rule1 (PasswordCheck minOccurences maxOccurence c subject)
  | occurences >= minOccurences && occurences <= maxOccurence = True
  | otherwise = False
  where occurences = length $ filter (==c) subject

rule2 :: PasswordCheck -> Bool
rule2 (PasswordCheck firstIndex secondIndex c subject)
  | firstMatch /= secondMatch = True
  | otherwise = False
  where firstMatch = subject !! (firstIndex - 1) == c
        secondMatch = subject !! (secondIndex - 1) == c

puzzle2 :: IO ()
puzzle2 = do  filePath <- getDataFileName "data/puzzle2-input.txt"
              contents <- readFile filePath
              let fileLines = lines contents
              let checks = parsePasswordCheck <$> fileLines

              print $ length $ filter (== True) $ map (`runRule` rule1) checks
              print $ length $ filter (== True) $ map (`runRule` rule2) checks

parsePasswordCheck :: String -> Maybe PasswordCheck
parsePasswordCheck s = do
              let x = runParser parser () "" s
              case x of
                Left e  -> error $ show e
                Right y -> Just y

runRule :: Maybe PasswordCheck -> (PasswordCheck -> Bool) -> Bool
runRule check rule = maybe False rule check