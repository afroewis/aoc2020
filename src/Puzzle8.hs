module Puzzle8 (puzzle8) where

import Paths_aoc2020
import Data.Text.Read
import Data.Text (pack)

data Instruction = Instruction {instr :: InstructionType, val :: Int} deriving (Eq)
instance Show Instruction where
  show (Instruction type' val) = show type' ++ " " ++ show val

data ProgramState = ProgramState {state_instr :: Int, state_val :: Int}
data InstructionType = Jmp | Acc | Nop deriving (Eq, Show)

execute :: [Instruction] -> [Int] -> Int -> Int -> Int
execute instrs ranInstrs i acc = if length (filter (==i) ranInstrs') == 2
                                 then acc
                                 else execute instrs ranInstrs' (state_instr newState) (state_val newState)
   where newState = executeInstruction currInstr currState
         currState = ProgramState i acc
         currInstr = instrs !! i
         ranInstrs' = ranInstrs ++ [i]

executeInstruction :: Instruction -> ProgramState -> ProgramState
executeInstruction instruction state = ProgramState nextInstr nextVal
  where nextInstr = if instr instruction == Jmp
                    then state_instr state + val instruction
                    else state_instr state + 1
        nextVal = if instr instruction == Acc
                            then state_val state + val instruction
                            else state_val state

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
                 Right r -> fst r
                where reader = signed decimal $ pack s

puzzle8 :: IO()
puzzle8 = do
     filePath <- getDataFileName "data/puzzle8-input.txt"
     contents <- readFile filePath
     let fileLines = lines contents
     let instructions = map parseLine fileLines

     print $ execute instructions [] 0 0