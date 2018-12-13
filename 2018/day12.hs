module Day12 where

import Data.List
import Data.Char
import Data.Maybe
import Data.Array

type Pot = Char

inputData, iD :: [Pot]
inputData = "#...#..###.#.###.####.####.#..#.##..#..##..#.....#.#.#.##.#...###.#..##..#.##..###..#..##.#..##..."
iD = replicate (length inputData) '.' ++ inputData ++ replicate (length inputData * 2) '.'

match :: [Pot] -> Pot
match "...#." = '#'
match "#..##" = '#'
match "....." = '.'
match "##.##" = '.'
match ".##.." = '#'
match ".##.#" = '.'
match "####." = '#'
match ".#.#." = '.'
match "..#.#" = '.'
match ".#.##" = '.'
match ".#..#" = '.'
match "##..." = '#'
match "#...#" = '#'
match "#####" = '.'
match "#.###" = '#'
match "..###" = '#'
match "###.." = '.'
match "#.#.#" = '#'
match "##..#" = '#'
match "..#.." = '#'
match ".####" = '.'
match "#.##." = '.'
match "....#" = '.'
match "...##" = '.'
match "#...." = '.'
match "#..#." = '.'
match "..##." = '.'
match "###.#" = '#'
match "#.#.." = '#'
match "##.#." = '#'
match ".###." = '.'
match ".#..." = '.'
match _ = '.'

subString :: [Pot]-> Int -> [Pot]
subString str i = (take 5 . drop (i - 2 + length inputData)) str
                          
evolveRow :: [Pot] -> [Pot]
evolveRow row = map (match . subString row) [(negate l) .. (l*3)]
            where l = length inputData
            
evolver :: [Pot] -> Int -> [Pot]
evolver row 0 = row
evolver row i = evolver (evolveRow row) (i - 1)

--failed attempt in finding a loop
findFixedPoint :: [Pot] -> [[Pot]] -> [Pot]
findFixedPoint row seen | evolved `elem` seen = evolved
                        | otherwise = findFixedPoint evolved (evolved:seen)
                        where evolved = evolveRow row

plantSum :: [Pot] -> Int -> Int
plantSum [] _ = 0
plantSum (x:xs) i   | x == '#' = i + plantSum xs (i+1)
                    | otherwise = plantSum xs (i+1)


-- At 100 there is an steady increase of 22 per evolve
f1 = plantSum (evolver iD 100) (negate (length inputData)) -- results 2675
f2 = plantSum (evolver iD 101) (negate (length inputData)) -- 2697
f3 = plantSum (evolver iD 102) (negate (length inputData)) -- 2719
f4 = plantSum (evolver iD 103) (negate (length inputData)) -- 2741

-- Value at 100 + (50000000000 - 100) left iterations * increase per evolve
resultP1 :: Int
resultP1 = plantSum (evolver iD 20) ((negate . length) inputData)
resultP2 = 2675 + (50000000000 - 100) * 22