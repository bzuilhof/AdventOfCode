{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day11 where
import Data.Map (Map, empty, (!), fromList, union, toList, findWithDefault, adjust, insert)
inputDataTest = ["5483143223","2745854711","5264556173","6141336146","6357385478","4167524645","2176841721","6882881134","4846848554","5283751526"]
inputData = ["5433566276","6376253438","8458636316","6253254525","7211137138","1411526532","5788761424","8677841514","1622331631","5876712227"]
inputMini = ["90","00"]

type Coordinate = (Int,Int)
getRowCoordinates :: String -> Int -> Map Coordinate Int
getRowCoordinates r y = fromList (zipWith (\x v -> ((x,y), read [v])) [0..length r] r)

getCoordinates :: [String] -> Int -> Map Coordinate Int
getCoordinates  [] _ = empty
getCoordinates (x:xs) y =  getRowCoordinates x y `union` getCoordinates xs (y+1)

getMap :: [String] -> Map Coordinate Int
getMap input = getCoordinates input 0

getNeighbourPoints :: Coordinate -> [Coordinate]
getNeighbourPoints (x,y) = [(x-1, y), (x+1,y), (x,y-1), (x, y+1)]



evolveMap :: [Coordinate] -> Map Coordinate Int -> Map Coordinate Int
evolveMap xs m = foldl (flip (adjust (+ 1))) m xs

getFlashes :: [Coordinate] -> Map Coordinate Int -> Map Coordinate Int
getFlashes [] m = m
getFlashes (x:xs) m | v == 9 = getFlashes (neighbours ++ xs) newMap
                    | otherwise = getFlashes xs m
                where 
                    v = findWithDefault 0 x m
                    neighbours = getNeighbourPoints x
                    newMap = foldl (flip (adjust (+ 1))) (insert x 0 m) xs  



part1 :: Map Coordinate Int
part1 = evolveMap possibleCoords cmap

step2 = evolveMap possibleCoords $ evolveMap possibleCoords cmap
possibleCoords = map fst $ toList cmap
cmap = getMap inputDataTest