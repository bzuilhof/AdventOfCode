module Day2 where

import Data.List
import Data.Char
import Data.Sequence as Seq

inputData :: Seq Integer
inputData = fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,5,19,23,1,13,23,27,1,6,27,31,2,31,13,35,1,9,35,39,2,39,13,43,1,43,10,47,1,47,13,51,2,13,51,55,1,55,9,59,1,59,5,63,1,6,63,67,1,13,67,71,2,71,10,75,1,6,75,79,1,79,10,83,1,5,83,87,2,10,87,91,1,6,91,95,1,9,95,99,1,99,9,103,2,103,10,107,1,5,107,111,1,9,111,115,2,13,115,119,1,119,10,123,1,123,10,127,2,127,10,131,1,5,131,135,1,10,135,139,1,139,2,143,1,6,143,0,99,2,14,0,0]

evolveProgram :: Integer -> Seq Integer -> Seq Integer
evolveProgram cursor program    | mode == 99 = program
                                | mode == 1 = evolveProgram (cursor + 4) (applier program target (+) val1 val2)
                                | mode == 2 = evolveProgram (cursor + 4) (applier program target (*) val1 val2)
                            where   mode = indexI program cursor
                                    val1 = indexI program (indexI program (cursor + 1))
                                    val2 = indexI program (indexI program (cursor + 2))
                                    target = indexI program (cursor + 3)                                  


applier :: Seq Integer -> Integer -> (Integer -> Integer -> Integer) -> Integer -> Integer -> Seq Integer
applier program target f val1 val2 = updateI target (f val1 val2) program

indexI :: Seq a -> Integer -> a
indexI sequence i = index sequence (fromInteger i)

updateI :: Integer -> a -> Seq a -> Seq a
updateI i = update (fromInteger i)

setNounVerb :: Integer -> Integer -> Seq Integer -> Seq Integer
setNounVerb noun verb sequence = updateI 2 verb (updateI 1 noun sequence)

getHeadForNounVerb :: (Integer,Integer) -> Integer
getHeadForNounVerb (noun,verb) = indexI (evolveProgram 0 (setNounVerb noun verb inputData)) 0

getComb :: [(Integer, Integer)] -> (Integer, Integer)
getComb [x] = x
getComb (x:xs)  | getHeadForNounVerb x == 19690720 = x
                | otherwise = getComb xs

part1 :: Integer
part1 = getHeadForNounVerb (12,2)

part2 :: Integer
part2 = fst tup * 100 + snd tup
        where tup = getComb [(noun, verb) | noun <-[0..99], verb <-[0..99]]
