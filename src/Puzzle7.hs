module Puzzle7 (puzzle7) where

import Paths_aoc2020
import Text.Parsec (digit, runParser, optional, letter, many1, char, (<|>), sepBy1, string)
import Data.List (nub)
import Text.Parsec.String (Parser)
import Data.Maybe (fromJust)
import Control.Arrow (second)

type Bag = String

parseLine :: Parser (Bag, [(Int, Bag)])
parseLine = do
  b <- parseBag
  string " contain "
  bs <- (string "no other bags" >> return []) <|> (parseBags `sepBy1` string ", ")
  char '.'
  return (b, bs)

parseBag :: Parser Bag
parseBag = do
  d1 <- many1 letter
  char ' '
  d2 <- many1 letter
  string " bag"
  optional $ char 's'
  return $ d1 ++ ' ':d2

parseBags :: Parser (Int, Bag)
parseBags = do
  n <- read <$> many1 digit
  char ' '
  b <- parseBag
  return (n, b)

runParserAndIgnoreErrors :: String -> (Bag, [(Int, Bag)])
runParserAndIgnoreErrors str = case runParser parseLine () "" str of
                                Right y -> y

containers' :: [String] -> [(String, [(Int, String)])] -> [String]
containers' acc list = if new_containers == acc then acc else containers' new_containers list
    where containers bag = map fst $ filter (elem bag . map snd . snd) list
          new_containers = nub $ acc ++ (acc >>= containers)

capacity :: [(String, [(Int, String)])] -> String -> Int
capacity bags bag = if null storableBags then 0 else sum $ map (uncurry (*) . second ((1+) . capacity bags)) storableBags
    where storableBags = fromJust $ lookup bag bags

puzzle7 :: IO()
puzzle7 = do
     filePath <- getDataFileName "data/puzzle7-input.txt"
     contents <- readFile filePath
     let fileLines = lines contents
     let bags = runParserAndIgnoreErrors <$> fileLines
     putStrLn $ "Number of bags that can eventually contain a shiny golden bag: " ++ show (length (containers' ["shiny gold"] bags) - 1)
     putStrLn $ "Capacity of a shiny golden bag: " ++ show (capacity bags "shiny gold")