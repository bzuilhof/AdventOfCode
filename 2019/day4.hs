module Day4 where

import Data.List
import Data.Char


lb = 284639
ub = 748759


countValidPasswords :: [Int] -> Int
countValidPasswords [x] | isValidPassword' x = 1
                        | otherwise = 0
countValidPasswords (x:xs)  | isValidPassword' x = 1 + countValidPasswords xs
                            | otherwise = countValidPasswords xs

isValidPassword' :: Int -> Bool
isValidPassword' pw = compliesNotDesc pw && containsAdjMatchP2 pw

compliesNotDesc :: Int -> Bool 
compliesNotDesc pw = compliesNotDesc' (getLastNumber pw) (removeLastNumber (pw `mod` 100)) (pw `div` 100)

compliesNotDesc' :: Int -> Int -> Int -> Bool
compliesNotDesc' z y 0 = z >= y
compliesNotDesc' z y pw | z < y = False
                        | otherwise = compliesNotDesc' y (getLastNumber pw) (removeLastNumber pw)

containsAdjMatch :: Int -> Bool 
containsAdjMatch pw = containsAdjMatch' (getLastNumber pw) (removeLastNumber (pw `mod` 100)) (pw `div` 100)

containsAdjMatch' :: Int -> Int -> Int -> Bool
containsAdjMatch' z y 0 = z == y
containsAdjMatch' z y pw    | z == y = True
                            | otherwise = containsAdjMatch' y (getLastNumber pw) (removeLastNumber pw)

containsAdjMatchP2 :: Int -> Bool 
containsAdjMatchP2 pw = any (\x -> length x == 2) $ group (digs pw)
-- containsAdjMatchP2 pw = True

containsAdjMatchP2' :: [Int] -> Bool
containsAdjMatchP2' (a:b:c:d:e:f)   | a == b && b /= c = True
                                    | b == c && a /= b && c /= d = True
                                    | c == d && b /= c && d /= e = True
                                    | d == e && c /= d && (e /= head f) = True
                                    | (e == head f) && e /= d = True
                                    | otherwise = False
part1 :: Int
part1 = countValidPasswords [lb..ub]

removeLastNumber, removeTwoLast, getLastNumber, getSecondLastNumber :: Int -> Int
removeLastNumber x = x `div` 10
getLastNumber x = x `mod` 10
removeTwoLast x = x `div` 100
getSecondLastNumber x = removeLastNumber (x `mod` 100)

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]