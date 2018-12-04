module Day2 where

import Data.List
import Data.Char

inputData :: [[Int]]
inputData = [[2083,3,56,44,53,1,21,31,56,23,50,36,37,18,41,20,48,54,59,25,44,48,58,29,36,44,53,38,40,4,20,33,59,51,53],[3011,32,44,36,46,14,59,8,50,17,30,36,49,19,39,24,57,27,48,27,49,17,44,33,59,10,41,16,53,12,55,26,57,18,32,45,55],[2437,23,57,19,33,44,52,27,30,33,42,19,49,53,58,19,40,48,58,4,34,39,49,23,35,50,58,15,51,17,20,43,58,20,57,8,27,42,48,55,57,22,53,16,54,5,57,15,47,12,20,36,55],[1447,17,53,16,18,26,28,27,29,19,24,35,40,46,47,10,54,21,56,2,12,22,25,30,35,44,58,19,58,27,47,57,59,6,48,6,58,22,57,25,53,7,32,42,55,20,55,38,46,49,56,19,27,31,39,53,54],[1831,14,25,32,50,41,53,11,12,21,26,42,52,1,20,49,50,54,57,15,30,35,47,3,42,8,51,16,46],[3307,40,59,12,15,13,17,22,37,49,55,1,12,18,38,44,55,44,45,49,59,40,46,55,58,10,16,17,26,55,58,51,57,17,49,3,53,24,32,52,57,5,16,46,54,57,59,57,58,43,58,35,52,56,59,49,56],[523,37,54,1,13,24,48,19,37,0,22,28,39,40,42,48,51,34,57,18,46,14,42,14,46,52,57,14,27,30,48],[1901,22,58,39,49,13,59,1,22,7,46,51,58,2,32,7,44,57,58,30,48,55,56,4,29,41,46,32,46,8,29,27,48,3,48,31,51,5,50,54,56,8,34,38,45,4,52,30,52,38,42],[1009,0,44,14,43,33,39,45,50,30,34,32,56,24,33,42,52,8,34,37,59,37,50],[3251,2,19,29,59,8,55,32,47,9,35,42,59,10,18,36,57,5,51,32,39,42,52,57,59,17,58,6,45,37,47],[659,32,51,15,34,49,50,53,58,31,52,44,58,40,51,35,57,46,48,52,54,25,58],[1303,27,35,53,59,4,35,7,48,4,36,15,23,28,41,45,50,42,52,56,59],[2617,36,42,8,37,18,54,33,39,0,24,34,45,5,41,17,29,44,53,57,59,14,17,39,44,54,58],[3323,31,40,43,50,30,56,25,31,41,57,20,43,16,45,30,54,41,47,51,59,38,45,7,56,36,38,13,24,28,34,38,51,33,39,47,59,28,47,26,37,42,46],[1307,10,36,40,51,14,47,23,28,43,51,54,57,25,36,48,56,24,58,0,41,25,30,54,58,21,22,39,47,54,59,23,41,26,31,46,53,2,24,27,56,24,29,46,58,4,51,10,31,27,32,51,53,26,28,45,50,1,34,49,57,23,33,1,58],[1979,24,48,17,58,31,38,6,21,42,55,14,29,35,55,16,38,10,23,48,54,57,58,46,59,6,27,6,8,40,45,53,54,16,53,24,44,11,20,23,43,32,37,44,48,14,50,42,49],[2269,37,45,37,59,43,52,2,46,53,58,1,32,35,46,12,29,46,53,26,51,54,59,20,21,42,45,5,40,54,58],[2239,19,37,44,54,10,21,25,26,31,45,12,44,49,50,53,58,30,45,26,33,9,10,20,39,35,55,19,31,36,41,21,42,6,44,44,57,13,40,27,34,16,32,55,59,9,19,33,34],[991,15,34,16,51,27,58,17,57,21,47,19,55,2,24,29,37,5,22,26,58,14,27,29,38,43,53,57,58,45,53,15,40,20,33,45,55,24,51,32,41,27,48,18,37,51,58],[127,36,48,6,29,50,55,35,54,45,57,33,35,39,54,1,18,35,39,18,49,52,53,56,58,4,34,40,55,20,48,53,58,40,51,56,59,34,57,16,40,46,55]]

calcTotalSleep :: [Int] -> (Int,Int)
calcTotalSleep (x:xs) = (x,calcTotalSleep' xs (-1))

calcTotalSleep' :: [Int] -> Int -> Int
calcTotalSleep' [] _ = 0
calcTotalSleep' (x:xs) start    | start == -1 = calcTotalSleep' xs x
                                | otherwise = (x - start) + calcTotalSleep' xs (-1)

sleepiest :: [[Int]] -> Int -> Int -> Int
sleepiest [] g s = g
sleepiest (x:xs) mg ms  | ms < sleep = sleepiest xs guard sleep
                        | otherwise = sleepiest xs mg ms
                    where (guard,sleep) = calcTotalSleep x

sleepiestScheme :: [[Int]] -> [Int]
sleepiestScheme xs = sleepiestScheme' xs (sleepiest xs 0 0)                     
                    

sleepiestScheme' :: [[Int]] -> Int -> [Int]
sleepiestScheme' (x:xs) sleepiestG | guardId == sleepiestG = scheme
                        | otherwise = sleepiestScheme xs
                        where (guardId:scheme) = x

getRanges :: [Int] -> Int -> [[Int]]
getRanges [] _ = []
getRanges (x:xs) start  | start == (-1) = getRanges xs x
                        | otherwise = [start .. x] : getRanges xs (-1)

findMostOcc :: [[Int]] -> [Int] -> [Int]
findMostOcc [] x = x
findMostOcc (x:xs) ys   | length x > length ys = findMostOcc xs x
                        | otherwise = findMostOcc xs ys

getShiftData :: [Int] -> (Int, [Int])
getShiftData (guardId:times) = (guardId, minutes)
                            where minutes = findMostOcc (flattenTimes (getRanges times (-1))) []

flattenTimes :: [[Int]] -> [[Int]]
flattenTimes = group . sort . concat


mostOccurer :: [(Int, [Int])] -> (Int, [Int])
mostOccurer xs = mostOccurer' xs (0,[])

mostOccurer' :: [(Int, [Int])] -> (Int, [Int]) -> (Int, [Int])
mostOccurer' [] (g,m) = (g,m)
mostOccurer' (x:xs) (mg,mm) | length m  > length mm = mostOccurer' xs (g,m)
                            | otherwise = mostOccurer' xs (mg, mm)
                            where (g,m) = x

resultP1, resultP2 :: Int
resultP1 = sleepiest inputData 0 0 * head (findMostOcc (flattenTimes (getRanges (sleepiestScheme inputData) (-1))) [])
resultP2 = g * head m
    where (g, m) = mostOccurer (map getShiftData inputData)
                