module Day6 where

import Data.List
import Data.Char
import Data.Maybe

type Coord = (Integer, Integer)
inputData :: [Coord]
inputData = [(227,133),(140,168),(99,112),(318,95),(219,266),(134,144),(306,301),(189,188),(58,334),(337,117),(255,73),(245,144),(102,257),(255,353),(303,216),(141,167),(40,321),(201,50),(60,188),(132,74),(125,199),(176,307),(204,218),(338,323),(276,278),(292,229),(109,228),(85,305),(86,343),(97,254),(182,151),(110,292),(285,124),(43,223),(153,188),(285,136),(334,203),(84,243),(92,185),(330,223),(259,275),(106,199),(183,205),(188,212),(231,150),(158,95),(174,212),(279,97),(172,131),(247,320)]

grid :: Integer -> [Coord]
grid size = [(x,y) | x <- [lB..uB], y <- [lB..uB]]
        where     lB = 0
                  uB = size

md :: Coord -> Coord -> Integer
md (x1,y1) (x2,y2) =  abs (x1 - x2) + abs (y1-y2)

closestCoord :: Coord -> [Coord] -> Maybe Coord
closestCoord p xs = closestCoord' p xs (Just p) 400

closestCoord' :: Coord -> [Coord] -> Maybe Coord -> Integer-> Maybe Coord
closestCoord' _ [] c _ = c 
closestCoord' p (x:xs) c m   | dist < m = closestCoord' p xs (Just x) dist
                            | dist == m = closestCoord' p xs Nothing m
                            | otherwise = closestCoord' p xs c m
                            where dist = md x p

calcGrid :: [Coord] -> [Coord] -> [Coord]
calcGrid input = mapMaybe (`closestCoord` input)

getBorderCoord :: Integer -> [Coord]
getBorderCoord size = [(x,y) | x <- [lB,uB], y <- [lB..uB]] ++ [(x,y) | x <- [lB..uB], y <- [lB,uB]]
                              where lB = 0
                                    uB = size

totalDist :: Coord -> Integer
totalDist c = sum (map (md c) inputData)

resultP1, resultP2 :: Int
resultP1 = maximum (map length ((group . sort) (filter (`notElem` border) processedGrid)))
                  where border = calcGrid inputData (getBorderCoord 400)
                        processedGrid = calcGrid inputData (grid 400)

resultP2 = length (filter (\x -> totalDist x < 10000) (grid 400))



