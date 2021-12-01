module Day19 where

import Data.Map.Strict (Map)

rules :: Map Int [Int]
rules = null

messages :: [String]
messages

isValid :: Map Int [Int] -> String -> Bool
isValid _ _ = False



part1 = length $ filter (isValid rules) messages