module Day9 where

import Data.List
import Data.Char
import Data.Maybe
import Data.Sequence as Seq

type Marble = Int

playerAmount, marbles :: Int
playerAmount = 431
marbles = 70950

testSeq :: Seq Int
testSeq = fromList [1..10]

removeAt :: Seq Marble -> Int -> Seq Marble
removeAt circle pos = deleteAt pos circle

increaseScore :: Seq Int -> Int -> Int -> Seq Int
increaseScore players p s = adjust (+ s) p players

getPos :: Int -> Seq Marble -> Int 
getPos pos circle   | pos > l = getPos (pos - l) circle
                    | pos < 0 = getPos (pos + l) circle
                    | otherwise = pos
                        where l = Seq.length circle

playNext :: Seq Marble -> Marble -> Int -> Seq Marble
playNext circle marble pos = insertAt pos marble circle

play :: [Marble] -> Seq Marble ->  Int -> Seq Marble -> Seq Marble
play [] circle _ scores = scores
play (x:xs) circle pos scores   | x `mod` 23 == 0 = play xs removedCircle (backPos + 2) newScores
                                | otherwise = play xs nextCircle (realPos + 2) scores
                        where   nextCircle = playNext circle x realPos 
                                realPos = getPos pos circle
                                backPos = getPos (pos - 9) circle
                                newScores = increaseScore scores (x `mod` playerAmount) increment
                                increment = x + (circle `index` backPos)
                                removedCircle = removeAt circle backPos

resultP1, resultP2 :: Int
resultP1 = maximum (play [1..marbles] (singleton 0) 2 (Seq.replicate playerAmount 0))
resultP2 = maximum (play [1..marbles*100] (singleton 0) 2 (Seq.replicate playerAmount 0))

