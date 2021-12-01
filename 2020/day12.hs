module Day12 where

inputData :: [Instruction]
inputData = [('F',10),('N',3),('F',7),('R',90),('F',11)]

type Instruction = (Char,Int)

sail :: (Int, Int) -> Char -> [Instruction] -> (Int, Int)
sail coord _ [] = coord
sail coord ori (x:xs) = sail newLoc newOri xs 
                    where (newLoc,newOri) = updateLocation coord x ori
                

updateLocation :: (Int, Int) -> Instruction -> Char -> ((Int,Int), Char)
updateLocation (x,y) i@(d,n) cd 
                              | d == 'L' || d == 'R' = ((x,y), direction)
                              | direction == 'N' = ((x, y + n), direction)
                              | direction == 'E' = ((x + n, y), direction)
                              | direction == 'S' = ((x, y - n), direction)
                              | direction == 'W' = ((x - n, y), direction)
                              where direction = getDirection i cd


getDirection :: Instruction -> Char -> Char 
getDirection (d,n) cd | (d == 'F' && cd == 'N') || d == 'N' = 'N'
                      | (d == 'F' && cd == 'E') || d == 'E' = 'E'
                      | (d == 'F' && cd == 'S') || d == 'S' = 'S'
                      | (d == 'F' && cd == 'W') || d == 'W' = 'W'
                      | d == 'R' = getDirectionByRotation cd n
                      | d == 'L' = getDirectionByRotation cd (-n)

getDirectionByRotation :: Char -> Int -> Char
getDirectionByRotation d x =  numberToDir ((dirToNumber d + x ) `mod` 360)

dirToNumber :: Char -> Int
dirToNumber 'N' = 0
dirToNumber 'E' = 90
dirToNumber 'S' = 180
dirToNumber 'W' = 270

numberToDir :: Int -> Char
numberToDir 0 = 'N'
numberToDir 90 = 'E' 
numberToDir 180 = 'S'
numberToDir 270 = 'W'

part1 = sail (10,0) 'E' [('N',3),('F',7),('R', 90),('F', 11)]
partx = sail (10,3) 'E' [('F',7),('R', 90),('F', 11)]
