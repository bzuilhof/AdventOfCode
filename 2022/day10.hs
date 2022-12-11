module Day10 where
import GHC.Float (powerDouble)
import Data.List
import Data.List.Split

testBig = ["addx 15","addx -11","addx 6","addx -3","addx 5","addx -1","addx -8","addx 13","addx 4","noop","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx -35","addx 1","addx 24","addx -19","addx 1","addx 16","addx -11","noop","noop","addx 21","addx -15","noop","noop","addx -3","addx 9","addx 1","addx -3","addx 8","addx 1","addx 5","noop","noop","noop","noop","noop","addx -36","noop","addx 1","addx 7","noop","noop","noop","addx 2","addx 6","noop","noop","noop","noop","noop","addx 1","noop","noop","addx 7","addx 1","noop","addx -13","addx 13","addx 7","noop","addx 1","addx -33","noop","noop","noop","addx 2","noop","noop","noop","addx 8","noop","addx -1","addx 2","addx 1","noop","addx 17","addx -9","addx 1","addx 1","addx -3","addx 11","noop","noop","addx 1","noop","addx 1","noop","noop","addx -13","addx -19","addx 1","addx 3","addx 26","addx -30","addx 12","addx -1","addx 3","addx 1","noop","noop","noop","addx -9","addx 18","addx 1","addx 2","noop","noop","addx 9","noop","noop","noop","addx -1","addx 2","addx -37","addx 1","addx 3","noop","addx 15","addx -21","addx 22","addx -6","addx 1","noop","addx 2","addx 1","noop","addx -10","noop","noop","addx 20","addx 1","addx 2","addx 2","addx -6","addx -11","noop","noop","noop"]
inputData = ["addx 1","noop","addx 2","noop","addx 3","addx 3","addx 1","addx 5","addx 1","noop","noop","addx 4","noop","noop","addx -9","addx 16","addx -1","noop","addx 5","addx -2","addx 4","addx -35","addx 2","addx 28","noop","addx -23","addx 3","addx -2","addx 2","addx 5","addx -8","addx 19","addx -8","addx 2","addx 5","addx 5","addx -14","addx 12","addx 2","addx 5","addx 2","addx -13","addx -23","noop","addx 1","addx 5","addx -1","addx 2","addx 4","addx -9","addx 10","noop","addx 6","addx -11","addx 12","addx 5","addx -25","addx 30","addx -2","addx 2","addx -5","addx 12","addx -37","noop","noop","noop","addx 24","addx -17","noop","addx 33","addx -32","addx 3","addx 1","noop","addx 6","addx -13","addx 17","noop","noop","noop","addx 12","addx -4","addx -2","addx 2","addx 3","addx 4","addx -35","addx -2","noop","addx 20","addx -13","addx -2","addx 5","addx 2","addx 23","addx -18","addx -2","addx 17","addx -10","addx 17","noop","addx -12","addx 3","addx -2","addx 2","noop","addx 3","addx 2","noop","addx -13","addx -20","noop","addx 1","addx 2","addx 5","addx 2","addx 5","noop","noop","noop","noop","noop","addx 1","addx 2","addx -18","noop","addx 26","addx -1","addx 6","noop","noop","noop","addx 4","addx 1","noop","noop","noop","noop"]
testSmall = ["noop","addx 3","addx -5"]
cycles = [20, 60, 100, 140, 180, 220]

runInstruction :: Int -> Int -> Bool -> [String] -> [Int]
runInstruction x c d [] = []
runInstruction x c d (y:ys) | i == "addx" && d = s : runInstruction x (c + 1) False (y:ys)
                            | i == "addx" && not d = s : runInstruction (x + n) (c + 1) True ys
                            | i == "noop" = s : runInstruction x (c + 1) True ys
                            | otherwise = []
                            where
                                i = head (words y)
                                n = read (words y !! 1) :: Int
                                s = if c `elem` cycles then c * x else 0

runInstructionP2 :: Int -> Int -> Bool -> [String] -> [Char]
runInstructionP2 x c d [] = []
runInstructionP2 x c d (y:ys) | i == "addx" && d = p : runInstructionP2 x (c + 1) False (y:ys)
                            | i == "addx" && not d = p : runInstructionP2 (x + n) (c + 1) True ys
                            | i == "noop" = p : runInstructionP2 x (c + 1) True ys
                            | otherwise = []
                            where
                                i = head (words y)
                                n = read (words y !! 1) :: Int
                                p = if (c `mod` 40) `elem` [x-1, x, x+1] then 'X' else ' '
                                

part1 = sum $ runInstruction 1 1 True inputData
part2 = mapM_ putStrLn $ chunksOf 40 $ runInstructionP2 2 1 True inputData