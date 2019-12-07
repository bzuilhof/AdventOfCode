module Day7 where
    
import Data.Map (Map)
import Data.Tuple
import Data.List
import Data.Sequence as Seq
import Data.Bool

inputData, testData, testData2 :: Seq Int
inputData = fromList [3,8,1001,8,10,8,105,1,0,0,21,46,55,76,89,106,187,268,349,430,99999,3,9,101,4,9,9,1002,9,2,9,101,5,9,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,1002,9,5,9,4,9,99,3,9,1001,9,2,9,1002,9,4,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,99]
testData = fromList [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
testData2 = fromList [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
evolveProgram :: Int -> Seq Int -> [Int] -> Seq Int
evolveProgram cursor program io | mode == 99 = fromList io
                                | mode == 1 = evolveProgram (cursor + 4) (applier program target (+) val1 val2) io
                                | mode == 2 = evolveProgram (cursor + 4) (applier program target (*) val1 val2) io
                                | mode == 3 = evolveProgram (cursor + 2) (update p1 (head io) program) (tail io)
                                | mode == 4 = evolveProgram (cursor + 2) program (index program p1 : io)
                                | mode == 5 = evolveProgram (bool (cursor + 3) val2 (val1 /= 0)) program io
                                | mode == 6 = evolveProgram (bool (cursor + 3) val2 (val1 == 0)) program io
                                | mode == 7 = evolveProgram (cursor + 4) (update target (bool 0 1 (val1 < val2)) program) io
                                | mode == 8 = evolveProgram (cursor + 4) (update target (bool 0 1 (val1 == val2)) program) io
                                | mode == 9 = fromList [99999]
                                where   command = index program cursor
                                        p1 = index program (cursor + 1)
                                        p2 = index program (cursor + 2)
                                        target = index program (cursor + 3)
                                        mode = getOpcode command
                                        p1Mode = getParamMode command 1
                                        p2Mode = getParamMode command 2
                                        val1 = getVal p1 p1Mode program
                                        val2 = getVal p2 p2Mode program

applier :: Seq Int -> Int -> (Int -> Int -> Int) -> Int -> Int -> Seq Int
applier program target f val1 val2 = update target (f val1 val2) program

getOpcode :: Int -> Int
getOpcode val = val `mod` 100

getParamMode :: Int -> Int -> Int
getParamMode val p = (val `mod` (10^(p+2))) `div` (10^(p+1))

getVal :: Int -> Int -> Seq Int -> Int
getVal val 0 program = index program val
getVal val 1 program = val

getPowerForSetting :: [Int] -> Int -> Seq Int -> Int
getPowerForSetting [x] input program = getOutput (evolveProgram 0 program [x,input])
getPowerForSetting (x:xs) input program = getPowerForSetting xs output program
                                        where
                                            output = getPowerForSetting [x] input program

getOutput :: Seq a -> a
getOutput seq = index seq 0

part1 = maximum (map (\x -> getPowerForSetting x 0 inputData) (permutations [0..4]))
part2 = maximum (map (\x -> getPowerForSetting x 0 inputData) (permutations [5..9]))