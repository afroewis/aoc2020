module Puzzle2
  ( puzzle2
  ) where

import Text.Parsec
import Text.Parsec.String
import Paths_aoc2020

data PasswordCheck = PasswordCheck Int Int Char String deriving Show

isValid :: PasswordCheck -> Bool
isValid (PasswordCheck minOcc maxOcc c subject)
  |occ >= minOcc && occ <= maxOcc = True
  |otherwise = False
  where occ = length $ filter (==c) subject

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
            _ <- oneOf ": "
            password <- str
            return (PasswordCheck minOcc maxOcc char password)

puzzle2 :: IO ()
puzzle2 = do  filePath <- getDataFileName "data/puzzle2-input.txt"
              contents <- readFile filePath
              let fileLines = lines contents
              let checks = parsePasswordCheck <$> fileLines
              print $ length $ filter runIsValidOnMaybe checks

parsePasswordCheck :: String -> Maybe PasswordCheck
parsePasswordCheck s = do
              let x = runParser parser () "" s
              case x of
                Left e  -> Nothing
                Right y ->  Just y

runIsValidOnMaybe :: Maybe PasswordCheck -> Bool
runIsValidOnMaybe = maybe False (isValid)