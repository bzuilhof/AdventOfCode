module Day5 where
import Day1 (input)

inputData :: [[Char]]
inputData = ["VJBD1","FDRWBVP2","QWCDLFGR3","BDNLMPJW4","QSCPBNH5","GNSBDR6","HSFQMPBZ7","FLW8","RMFVS9"]
testData = ["NZ1","DCM2","P3"]
testInstructions, inputInstructions :: [(Int, Int, Int)]
testInstructions = [(1, 2, 1),(3, 1, 3),(2, 2, 1),(1, 1, 2)]
inputInstructions = [(1, 4, 1),(2, 4, 8),(5, 9, 6),(1, 1, 3),(5, 8, 3),(1, 1, 5),(4, 3, 6),(14, 6, 2),(5, 4, 5),(7, 7, 2),(24, 2, 3),(13, 3, 2),(1, 7, 9),(1, 9, 5),(7, 2, 6),(3, 1, 7),(3, 6, 3),(2, 7, 1),(1, 7, 5),(2, 2, 6),(2, 1, 4),(9, 5, 1),(1, 6, 3),(4, 5, 4),(1, 2, 7),(4, 6, 2),(7, 2, 3),(2, 2, 6),(2, 2, 3),(2, 5, 4),(1, 7, 3),(4, 6, 7),(19, 3, 6),(3, 7, 4),(1, 7, 8),(1, 8, 1),(2, 1, 3),(10, 3, 2),(3, 3, 8),(1, 3, 9),(1, 9, 6),(11, 6, 8),(2, 3, 8),(6, 4, 3),(3, 4, 1),(7, 2, 8),(1, 3, 6),(6, 8, 5),(1, 4, 6),(9, 6, 9),(6, 3, 8),(1, 3, 5),(10, 1, 3),(11, 8, 7),(1, 3, 5),(1, 1, 8),(5, 9, 2),(1, 6, 3),(5, 3, 6),(1, 3, 5),(4, 6, 4),(1, 5, 9),(6, 2, 4),(2, 2, 9),(5, 5, 1),(2, 1, 7),(10, 8, 3),(1, 8, 6),(3, 6, 3),(6, 4, 2),(8, 3, 8),(3, 4, 8),(4, 2, 1),(3, 5, 3),(4, 7, 6),(2, 9, 3),(1, 2, 9),(1, 2, 3),(2, 4, 8),(1, 7, 9),(5, 7, 8),(2, 7, 3),(14, 3, 2),(3, 9, 5),(1, 3, 1),(1, 7, 4),(3, 9, 8),(7, 8, 9),(7, 2, 5),(2, 3, 7),(2, 7, 6),(16, 8, 9),(4, 6, 5),(1, 2, 5),(21, 9, 5),(3, 9, 3),(6, 1, 4),(1, 1, 9),(1, 1, 4),(2, 6, 3),(3, 4, 6),(3, 4, 8),(1, 9, 4),(2, 4, 6),(4, 3, 6),(1, 3, 4),(1, 4, 9),(1, 9, 8),(1, 8, 6),(6, 2, 1),(2, 8, 4),(6, 1, 8),(23, 5, 9),(1, 4, 7),(1, 7, 1),(22, 9, 7),(4, 8, 7),(1, 5, 2),(1, 1, 9),(2, 8, 4),(6, 6, 3),(2, 9, 5),(18, 7, 4),(18, 4, 5),(1, 2, 7),(1, 8, 4),(6, 7, 2),(5, 4, 5),(1, 3, 1),(1, 7, 2),(4, 3, 4),(1, 3, 4),(1, 1, 7),(1, 5, 8),(3, 4, 3),(3, 3, 8),(2, 8, 3),(2, 4, 8),(2, 7, 5),(1, 7, 9),(2, 3, 1),(1, 9, 7),(4, 2, 3),(1, 8, 9),(2, 1, 8),(2, 2, 4),(1, 9, 1),(4, 6, 8),(1, 2, 7),(1, 4, 7),(4, 8, 2),(1, 4, 3),(1, 1, 9),(4, 8, 1),(2, 2, 1),(3, 3, 9),(2, 7, 1),(32, 5, 1),(1, 8, 7),(6, 5, 1),(2, 7, 6),(1, 9, 5),(1, 3, 2),(1, 5, 9),(2, 6, 1),(1, 3, 7),(1, 9, 8),(36, 1, 4),(1, 8, 9),(5, 4, 9),(6, 9, 3),(2, 2, 9),(3, 1, 9),(1, 3, 2),(30, 4, 8),(1, 7, 5),(1, 3, 5),(3, 3, 4),(2, 8, 5),(3, 9, 8),(3, 9, 3),(19, 8, 6),(2, 3, 5),(3, 4, 3),(1, 4, 7),(8, 1, 8),(1, 3, 2),(1, 7, 6),(4, 5, 3),(1, 1, 7),(2, 5, 4),(1, 9, 4),(12, 6, 2),(1, 7, 8),(6, 2, 9),(3, 6, 7),(2, 7, 5),(6, 2, 3),(8, 3, 5),(5, 6, 8),(5, 3, 6),(1, 9, 4),(1, 9, 8),(5, 5, 9),(3, 4, 6),(1, 4, 9),(1, 7, 5),(1, 3, 5),(8, 9, 2),(3, 9, 6),(27, 8, 2),(10, 6, 9),(1, 6, 4),(1, 4, 9),(2, 5, 6),(5, 5, 3),(2, 6, 9),(5, 3, 2),(12, 9, 3),(5, 3, 1),(3, 1, 5),(1, 9, 8),(1, 5, 2),(1, 2, 1),(1, 1, 6),(1, 5, 3),(34, 2, 4),(8, 3, 9),(1, 6, 1),(1, 8, 5),(4, 2, 8),(3, 8, 7),(1, 7, 2),(7, 9, 8),(1, 9, 6),(2, 5, 1),(1, 6, 9),(1, 9, 5),(2, 2, 5),(5, 8, 6),(2, 8, 5),(1, 1, 3),(12, 4, 6),(2, 7, 1),(4, 1, 6),(3, 2, 3),(1, 8, 5),(1, 2, 6),(1, 1, 9),(1, 9, 5),(16, 4, 1),(4, 3, 1),(8, 1, 8),(1, 4, 1),(6, 5, 8),(1, 5, 7),(12, 6, 9),(7, 1, 5),(2, 1, 7),(1, 7, 1),(9, 9, 6),(15, 6, 2),(2, 9, 7),(4, 4, 5),(2, 2, 9),(3, 7, 5),(2, 1, 3),(1, 7, 1),(10, 2, 3),(6, 8, 6),(3, 9, 2),(14, 5, 6),(1, 8, 4),(5, 8, 2),(2, 2, 3),(24, 6, 1),(3, 1, 2),(9, 2, 9),(1, 4, 3),(1, 4, 2),(1, 8, 4),(23, 1, 4),(3, 2, 4),(2, 1, 2),(1, 8, 4),(3, 3, 5),(3, 3, 4),(3, 5, 8),(3, 2, 7),(2, 3, 8),(15, 4, 3),(2, 4, 1),(19, 3, 9),(1, 7, 2),(1, 2, 5),(1, 5, 4),(1, 7, 6),(1, 7, 4),(3, 8, 3),(1, 8, 4),(5, 3, 8),(1, 3, 6),(22, 9, 2),(17, 2, 6),(3, 9, 3),(9, 4, 9),(6, 4, 9),(5, 2, 6),(1, 4, 2),(1, 4, 9),(1, 1, 6),(19, 9, 2),(4, 8, 7),(1, 1, 5),(1, 5, 3),(1, 8, 1),(1, 8, 2),(4, 3, 7),(12, 6, 1),(3, 7, 3),(7, 2, 7),(9, 2, 6),(4, 2, 6),(13, 1, 4),(8, 6, 4),(16, 4, 8),(12, 7, 6),(3, 8, 3),(1, 1, 2),(4, 3, 8),(5, 8, 9),(27, 6, 8),(2, 3, 7),(2, 2, 8),(2, 7, 5),(1, 5, 9),(1, 5, 1),(1, 6, 9),(2, 6, 2),(2, 2, 6),(2, 9, 2),(3, 4, 3),(1, 1, 9),(5, 9, 8),(1, 9, 5),(2, 2, 6),(2, 4, 6),(1, 3, 7),(1, 5, 6),(1, 6, 7),(6, 6, 8),(2, 7, 5),(2, 3, 2),(34, 8, 1),(1, 5, 6),(1, 5, 3),(1, 6, 1),(32, 1, 8),(23, 8, 4),(1, 2, 1),(24, 8, 4),(1, 3, 6),(47, 4, 6),(2, 6, 1),(3, 1, 5),(1, 2, 1),(3, 5, 7),(21, 6, 2),(3, 7, 8),(2, 1, 6),(8, 6, 4),(4, 8, 9),(3, 2, 8),(4, 4, 2),(2, 2, 5),(4, 9, 8),(2, 1, 5),(11, 6, 1),(14, 2, 6),(2, 4, 3),(1, 2, 9),(3, 2, 9),(20, 6, 5),(2, 4, 2),(4, 9, 1),(8, 8, 9),(1, 6, 9),(14, 5, 2),(10, 2, 7),(7, 9, 6),(1, 6, 8),(6, 2, 6),(1, 2, 5),(1, 3, 5),(9, 6, 3),(1, 5, 2),(9, 7, 3),(12, 3, 2),(9, 5, 9),(1, 8, 6),(3, 3, 5),(1, 7, 6),(14, 2, 6),(3, 9, 7),(6, 1, 2),(5, 1, 8),(10, 6, 9),(4, 5, 6),(3, 2, 4),(9, 9, 7),(1, 8, 7),(3, 9, 6),(3, 3, 7),(1, 5, 1),(15, 7, 1),(2, 8, 5),(2, 5, 4),(1, 7, 4),(1, 3, 1),(15, 6, 7),(2, 4, 9),(3, 4, 7),(18, 1, 6),(1, 8, 9),(6, 9, 7),(3, 6, 8),(1, 1, 2),(2, 9, 5),(2, 2, 9),(16, 6, 3),(15, 3, 7),(2, 8, 4),(1, 3, 7),(3, 4, 9),(2, 1, 9),(26, 7, 4),(1, 2, 1),(7, 9, 8),(1, 2, 5),(2, 5, 2),(8, 7, 5),(1, 7, 3),(1, 3, 9),(2, 2, 7),(1, 6, 4),(4, 8, 9),(1, 1, 3),(1, 5, 6),(2, 5, 7),(17, 4, 9),(6, 4, 9),(1, 3, 4),(6, 7, 9),(3, 5, 6),(2, 7, 9),(4, 8, 9),(4, 6, 4),(8, 4, 6),(1, 8, 4),(3, 5, 2),(2, 4, 3),(1, 7, 9),(2, 3, 5),(4, 6, 9),(1, 6, 1),(36, 9, 4),(2, 5, 3),(3, 2, 1),(3, 1, 4),(14, 4, 1),(1, 8, 5),(4, 1, 3),(5, 9, 5),(2, 5, 8),(1, 8, 9),(4, 9, 6),(3, 5, 8),(1, 5, 6),(2, 1, 6),(2, 9, 7),(6, 6, 4),(1, 1, 3),(29, 4, 6),(7, 3, 4),(1, 8, 9),(3, 1, 6),(4, 1, 4),(1, 8, 4),(4, 4, 3),(15, 6, 8),(9, 4, 9),(1, 7, 9),(8, 8, 3),(3, 6, 7),(1, 1, 2),(4, 7, 6),(7, 8, 5),(1, 8, 4),(2, 5, 7),(1, 2, 4),(5, 6, 1),(4, 3, 2)]

executeInstruction :: [[Char]] -> (Int, Int, Int) -> [[Char]]
executeInstruction xs (i, x, y)| i == 1 = newState
                                | otherwise = executeInstruction newState (i-1, x, y) 
                                where crate = head (xs !! (x-1))
                                      newFrom = tail (xs !! (x-1))
                                      newTo = crate : (xs !! (y-1))
                                      newState = replaceNth (y-1) newTo (replaceNth (x-1) newFrom xs)

executeInstructionp2 :: [[Char]] -> (Int, Int, Int) -> [[Char]]
executeInstructionp2 xs (i, x, y) =  replaceNth (y-1) newTo (replaceNth (x-1) newFrom xs)
                                where crates = take i (xs !! (x-1))
                                      newFrom = drop i (xs !! (x-1))
                                      newTo = crates ++ (xs !! (y-1))

replaceNth :: Int -> [Char] -> [[Char]] -> [[Char]]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs) | n == 0 = newVal:xs
                           | otherwise = x:replaceNth (n-1) newVal xs

part1, part2 :: String
part1 = map head $ foldl executeInstruction inputData inputInstructions
part2 = map head $ foldl executeInstructionp2 inputData inputInstructions