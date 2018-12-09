module Day7 where

import Data.List
import Data.Char
import Data.Maybe

type Task = Char
type Constraint = (Task, Task)
type Worker = (Task, Int)
--Step F must be finished before step E can begin.
inputData :: [Constraint]
inputData = [('C','P'),('V','Q'),('T','X'),('B','U'),('Z','O'),('P','I'),('D','G'),('A','Y'),('R','O'),('J','E'),('N','S'),('X','H'),('F','L'),('S','I'),('W','Q'),('H','K'),('K','Q'),('E','L'),('Q','O'),('U','G'),('L','O'),('Y','G'),('G','I'),('M','I'),('I','O'),('A','N'),('H','O'),('T','O'),('H','U'),('A','I'),('B','R'),('V','T'),('H','M'),('C','A'),('B','G'),('L','Y'),('T','J'),('A','R'),('X','L'),('B','L'),('A','F'),('K','O'),('W','M'),('Z','N'),('Z','S'),('R','K'),('Q','L'),('G','O'),('F','Y'),('V','H'),('E','I'),('W','Y'),('U','I'),('F','K'),('M','O'),('Z','H'),('X','S'),('J','O'),('B','I'),('F','H'),('D','U'),('E','M'),('Z','X'),('P','L'),('W','H'),('C','D'),('A','X'),('Q','I'),('R','Y'),('B','A'),('N','L'),('H','G'),('Y','M'),('L','G'),('G','M'),('Z','R'),('S','Q'),('P','J'),('V','J'),('J','I'),('J','X'),('W','O'),('B','F'),('R','M'),('V','S'),('R','W'),('H','E'),('E','U'),('X','Q'),('N','G'),('T','I'),('L','M'),('H','I'),('U','M'),('C','H'),('P','H'),('J','F'),('A','O'),('X','M'),('H','L'),('W','K')]
-- inputDataS = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
-- ['A','B','C','D','E','F'] 
getAvailableTask :: [Task] -> [Task] -> Task
getAvailableTask [] done = 'X'
getAvailableTask todo done = minimum avail
                        where   constraints = nub (map snd (getConstraints done))
                                avail = filter (`notElem` constraints) todo

getConstraints :: [Task] -> [Constraint]
getConstraints done = filter (\x -> fst x `notElem` done) inputData

calcTasks :: [Task] -> [Task] -> [Task]
calcTasks [x] _ = [x]
calcTasks todo done = firedTask : calcTasks todo' done'
                        where 
                            firedTask = getAvailableTask todo done
                            todo' = filter (/= firedTask) todo
                            done' = firedTask : done  
getCost :: Task -> Int
getCost task = fromJust (elemIndex task ['A'..'Z']) + 1


-- resultP2 = work 0 ('X',0) ('X',0) ['A'..'Z'] []

-- work :: Int -> Worker -> Worker -> [Task] -> [Task] -> [Task]
-- work time (ta1,ti1) (ta2,ti2) todo done | ti1 == 0 =

calcWorkers :: [Worker] -> [Task] -> [Task] -> ([Worker],[Task],[Task])
calcWorkers [] _ _ = ([],[],[])
calcWorkers (x:xs) todo done  | ti == 0 = mergeW (firedTask, getCost firedTask) (calcWorkers xs todo' done')
                            | otherwise = mergeW (ta, ti)(calcWorkers xs todo done)                                 
                            where 
                                (ta,ti) = x
                                firedTask = getAvailableTask todo done
                                todo' = filter (/= firedTask) todo
                                done' = firedTask : done

mergeW :: Worker -> ([Worker],[Task],[Task]) -> ([Worker],[Task],[Task])
mergeW w (workers,todo,done) = (w : workers, todo, done)