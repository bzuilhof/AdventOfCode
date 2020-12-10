module Day10 where

import Data.List

-- import Data.IntMap
-- import qualified Data.IntMap as IntMap
-- import qualified Data.List as List

inputData, inputDataTest1, inputDataTest2 :: [Int]
inputDataTest1 = [16,10,15,5,1,11,7,19,6,12,4]
inputDataTest2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
inputData = [114,51,122,26,121,90,20,113,8,138,57,44,135,76,134,15,21,119,52,118,107,99,73,72,106,41,129,83,19,66,132,56,32,79,27,115,112,58,102,64,50,2,39,3,77,85,103,140,28,133,78,34,13,61,25,35,89,40,7,24,33,96,108,71,11,128,92,111,55,80,91,31,70,101,14,18,12,4,84,125,120,100,65,86,93,67,139,1,47,38]

computeP1 :: [Int] -> Int -> Int -> Int
computeP1 [x,y] j1 j3 | y - x == 3 = j1 * (j3+1)
                      | y - x == 1 = (j1 + 1) *  j3
computeP1 (x:y:xs) j1 j3 | y - x == 3 = computeP1 (y:xs) j1 (j3+1)
                         |y - x == 1 = computeP1 (y:xs) (j1 + 1) j3  

jolts :: [Int] -> [Int]
jolts inputData = (0:inputData) ++ [maximum inputData + 3]


getPossiblePaths :: [Int] -> Int -> Int -> Int
getPossiblePaths xs goal index  | index >= l = 0
                          |  currentVal == goal = 1
                          | otherwise = sum (map (getPossiblePaths xs goal) validSuccIndex)
                          where 
                              l = length xs   
                              currentVal = xs !! index
                              validSuccIndex = filter (\x -> validDiff currentVal (xs !! x)) [index + 1..(min (l-1) (index + 3))]

validDiff :: Int -> Int -> Bool
validDiff x y | y - x <= 3 = True              
              | otherwise = False  

part1 :: Int
part1 = computeP1 ((sort . jolts) inputData) 0 0
part2 :: Int
part2 = getPossiblePaths adapters (maximum adapters) 0
            where adapters = jolts (sort inputData)