module Day7 where

import Data.List
import Data.Char
import Data.Maybe

type Task = Char
type Constraint = (Task, Task)
type Worker = (Task, Int)

inputData :: [Constraint]
inputData = [('C','P'),('V','Q'),('T','X'),('B','U'),('Z','O'),('P','I'),('D','G'),('A','Y'),('R','O'),('J','E'),('N','S'),('X','H'),('F','L'),('S','I'),('W','Q'),('H','K'),('K','Q'),('E','L'),('Q','O'),('U','G'),('L','O'),('Y','G'),('G','I'),('M','I'),('I','O'),('A','N'),('H','O'),('T','O'),('H','U'),('A','I'),('B','R'),('V','T'),('H','M'),('C','A'),('B','G'),('L','Y'),('T','J'),('A','R'),('X','L'),('B','L'),('A','F'),('K','O'),('W','M'),('Z','N'),('Z','S'),('R','K'),('Q','L'),('G','O'),('F','Y'),('V','H'),('E','I'),('W','Y'),('U','I'),('F','K'),('M','O'),('Z','H'),('X','S'),('J','O'),('B','I'),('F','H'),('D','U'),('E','M'),('Z','X'),('P','L'),('W','H'),('C','D'),('A','X'),('Q','I'),('R','Y'),('B','A'),('N','L'),('H','G'),('Y','M'),('L','G'),('G','M'),('Z','R'),('S','Q'),('P','J'),('V','J'),('J','I'),('J','X'),('W','O'),('B','F'),('R','M'),('V','S'),('R','W'),('H','E'),('E','U'),('X','Q'),('N','G'),('T','I'),('L','M'),('H','I'),('U','M'),('C','H'),('P','H'),('J','F'),('A','O'),('X','M'),('H','L'),('W','K')]

getAvailableTask :: [Task] -> [Task] -> Maybe Task
getAvailableTask [] done = Nothing
getAvailableTask todo done = getMaybeTask avail
                        where   constraints = nub (map snd (getConstraints done))
                                avail = filter (`notElem` constraints) todo

getMaybeTask :: [Task] -> Maybe Task
getMaybeTask tasks  | not (null tasks) = Just (minimum tasks)
                    | otherwise = Nothing

getConstraints :: [Task] -> [Constraint]
getConstraints done = filter (\x -> fst x `notElem` done) inputData

calcTasks :: [Task] -> [Task] -> [Task]
calcTasks [x] _ = [x]
calcTasks todo done = firedTask : calcTasks todo' done'
                        where 
                            firedTask = fromJust (getAvailableTask todo done)
                            todo' = filter (/= firedTask) todo
                            done' = firedTask : done  

calcWorkers :: [Worker] -> [Task] -> [Task] -> Int -> Int
calcWorkers workers todo done c | all (== ('-', 0)) workers && null todo = c
                                | isJust finishedTask = calcWorkers workers' todo done' c
                                | ('-',0) `elem` workers && isJust availableTask = calcWorkers workers'' todo' done c
                                | otherwise = calcWorkers (workerNext workers) todo done (c+1)
                                where 
                                    finishedTask = getReadyTask workers
                                    done' = fromJust finishedTask : done 
                                    workers' = removeTask workers (fromJust finishedTask)
                                    workers'' = assignTask workers (fromJust availableTask)
                                    availableTask = getAvailableTask todo done
                                    todo' = filter (/= fromJust availableTask) todo

workerNext :: [Worker] -> [Worker]
workerNext [] = []
workerNext (x:xs)   | ta /= '-' = (ta, ti-1) : workerNext xs
                    | otherwise = x : workerNext xs
                    where (ta, ti) = x

removeTask :: [Worker] -> Task -> [Worker]
removeTask [] _ = []
removeTask (x:xs) rta   | rta == ta = ('-',0) : xs
                        | otherwise = x : removeTask xs rta
                where (ta,ti) = x

getReadyTask :: [Worker] -> Maybe Task
getReadyTask [] = Nothing
getReadyTask (x:xs) | ta /= '-' && ti == 0= Just ta
                    | otherwise = getReadyTask xs
                    where (ta,ti) = x

assignTask :: [Worker] -> Task -> [Worker]
assignTask (x:xs) nt    | ta == '-' = (nt, cost) : xs
                        | otherwise = x : assignTask xs nt
                        where 
                            (ta,ti) = x
                            cost = fromJust (elemIndex nt ['A'..'Z']) + 61
resultP1 :: [Task]
resultP1 = calcTasks ['A'..'Z'] []

resultP2 :: Int
resultP2 = calcWorkers (replicate 5 ('-',0)) ['A'..'Z'] [] 0