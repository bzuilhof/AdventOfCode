module Day6 where
import Data.List


inputDataTest :: [Int]
inputDataTest = [3,4,3,1,2]
inputData :: [Int]
inputData = [4,5,3,2,3,3,2,4,2,1,2,4,5,2,2,2,4,1,1,1,5,1,1,2,5,2,1,1,4,4,5,5,1,2,1,1,5,3,5,2,4,3,2,4,5,3,2,1,4,1,3,1,2,4,1,1,4,1,4,2,5,1,4,3,5,2,4,5,4,2,2,5,1,1,2,4,1,4,4,1,1,3,1,2,3,2,5,5,1,1,5,2,4,2,2,4,1,1,1,4,2,2,3,1,2,4,5,4,5,4,2,3,1,4,1,3,1,2,3,3,2,4,3,3,3,1,4,2,3,4,2,1,5,4,2,4,4,3,2,1,5,3,1,4,1,1,5,4,2,4,2,2,4,4,4,1,4,2,4,1,1,3,5,1,5,5,1,3,2,2,3,5,3,1,1,4,4,1,3,3,3,5,1,1,2,5,5,5,2,4,1,5,1,2,1,1,1,4,3,1,5,2,3,1,3,1,4,1,3,5,4,5,1,3,4,2,1,5,1,3,4,5,5,2,1,2,1,1,1,4,3,1,4,2,3,1,3,5,1,4,5,3,1,3,3,2,2,1,5,5,4,3,2,1,5,1,3,1,3,5,1,1,2,1,1,1,5,2,1,1,3,2,1,5,5,5,1,1,5,1,4,1,5,4,2,4,5,2,4,3,2,5,4,1,1,2,4,3,2,1]

stepDay :: [Int] -> [Int]
stepDay [] = []
stepDay (0:xs) = [6,8] ++ stepDay xs
stepDay (x:xs) = (x-1) : stepDay xs

stepDay' :: [Int] -> [Int]
stepDay' [d0,d1,d2,d3,d4,d5,d6,d7,d8] = [d1,d2,d3,d4,d5,d6,d7+d0,d8,d0]

evolve :: Int -> [Int] -> [Int]
evolve 0 xs = xs
evolve n xs =  evolve (n-1) (stepDay xs)

evolve' :: Int -> [Int] -> [Int]
evolve' 0 xs = xs
evolve' n xs =  evolve' (n-1) (stepDay' xs)

buildMap :: Int -> [Int] -> [Int]
buildMap (-1) _ = []
buildMap n xs = buildMap (n-1) xs ++ [length (filter (== n) xs)]

part1,part2 :: Int
part1 = length $ evolve 80 inputData
part2 = sum $ evolve' 256 $ buildMap 8 inputData
