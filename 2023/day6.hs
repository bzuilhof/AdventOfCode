module Day6 where

races :: [(Int,Int)]
racesTest = [(7,9),(15,40),(30,200)]
racesTestP2 = [(71530,940200)]
races = [(53,250),(91,1330),(67,1081),(68,1025)]
racesP2 = [(53916768,250133010811025)]

getPossibleDistances :: Int -> [Int] -> [Int]
getPossibleDistances _ [] = []
getPossibleDistances m (p:xs) = d : (getPossibleDistances m xs)
                            where d = (m - p) * p

getPossibleWins :: (Int, Int) -> Int
getPossibleWins (t, r) = length $ filter (>r) (getPossibleDistances t [0..t])

part1 :: [(Int,Int)] -> Int
part1 rs = foldr1 (*) (map getPossibleWins rs)
