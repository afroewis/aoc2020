module Puzzle8 (puzzle8) where

import Paths_aoc2020
import Data.Text.Read
import Data.Text (pack)

data Instruction = Instruction {inst :: InstructionType, val :: Int}
instance Show Instruction where
  show (Instruction type' val) = show type' ++ " " ++ show val

data InstructionType = Jmp | Acc | Nop deriving (Show)

--execute :: [Instruction] -> Int -> Int -> Int
--execute instrs i acc = 3

parseLine :: String -> Instruction
parseLine s = Instruction type' val
    where type' = case fst foo  of
                    "jmp" -> Jmp
                    "acc" -> Acc
                    "nop" -> Nop
          val = readDecimal $ tail $ snd foo
          foo = splitAt 3 s

readDecimal :: String -> Int
readDecimal s = case reader of
                 Left _ -> 0
                 Right r -> fst r
                where reader = signed decimal $ pack s

puzzle8 :: IO()
puzzle8 = do
     filePath <- getDataFileName "data/puzzle8-input.txt"
     contents <- readFile filePath
     let fileLines = lines contents
     let instructions = map parseLine fileLines

     mapM_ print instructions