{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day7 where
import Data.List


testInput = ["cd /","ls","dir a","14848514 b.txt","8504156 c.dat","dir d","cd a","ls","dir e","29116 f","2557 g","62596 h.lst","cd e","ls","584 i","cd ..","cd ..","cd d","ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]

data Loc = FileP Int | Folder [Loc]

parse :: [String] -> [String]-> Loc -> Loc
parse [] _ t = t
parse (x:xs) path t | c == "cd" = parse xs (c:path) t
                    | c == "ls"  parse xs path t
                    
            where 
                split = words x
                c = head split



