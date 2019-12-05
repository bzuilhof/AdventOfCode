module Day2 where

import Data.List
import Data.Char
import Data.Sequence as Seq

inputData :: Seq Int
inputData = fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,1,13,23,27,1,6,27,31,2,31,13,35,1,9,35,39,2,39,13,43,1,43,10,47,1,47,13,51,2,13,51,55,1,55,9,59,1,59,5,63,1,6,63,67,1,13,67,71,2,71,10,75,1,6,75,79,1,79,10,83,1,5,83,87,2,10,87,91,1,6,91,95,1,9,95,99,1,99,9,103,2,103,10,107,1,5,107,111,1,9,111,115,2,13,115,119,1,119,10,123,1,123,10,127,2,127,10,131,1,5,131,135,1,10,135,139,1,139,2,143,1,6,143,0,99,2,14,0,0]

evolveProgram :: Int -> Seq Int -> Seq Int
evolveProgram cursor program    | mode == 99 = program
                                | mode == 1 = evolveProgram (cursor + 4) (applier program target (+) val1 val2)
                                | mode == 2 = evolveProgram (cursor + 4) (applier program target (*) val1 val2)
                                where   mode = index program cursor
                                        val1 = index program (index program (cursor + 1))
                                        val2 = index program (index program (cursor + 2))
                                        target = index program (cursor + 3)                                  

applier :: Seq Int -> Int -> (Int -> Int -> Int) -> Int -> Int -> Seq Int
applier program target f val1 val2 = update target (f val1 val2) program

setNounVerb :: Int -> Int -> Seq Int -> Seq Int
setNounVerb noun verb sequence = update 2 verb (update 1 noun sequence)

getHeadForNounVerb :: (Int,Int) -> Int
getHeadForNounVerb (noun,verb) = index (evolveProgram 0 (setNounVerb noun verb inputData)) 0

getComb :: [(Int, Int)] -> (Int, Int)
getComb [x] = x
getComb (x:xs)  | getHeadForNounVerb x == 19690720 = x
                | otherwise = getComb xs

part1 :: Int
part1 = getHeadForNounVerb (12,2)

part2 :: Int
part2 = fst tup * 100 + snd tup
        where tup = getComb [(noun, verb) | noun <-[0..99], verb <-[0..99]]
