module Day16 where

import qualified Data.Map as Map
import Data.Map (Map, union, empty, fromList, (!))

import qualified Data.Set as Set
import Data.Set (Set, insert, unions)

type Coordinate = (Int,Int)
type Beam = (Coordinate, Direction)
data Direction = North | South | East | West deriving (Eq, Ord, Show)
input = ["\\..|.....................|.-........\\.............-................|./....|.....-...-.........................",".......\\.........|..............-...||..............-...........--.|...............||..........\\............|.","........../.....................\\.........\\..................\\....|.....-.................../..-.\\............","................/..../........../..-....|\\.|...-...................\\....|/........|...........................",".....|../-\\.|....|................................--.-./..........|.........../..-....-................-....|.",".../..........\\................-.......\\................|/..\\.................\\.....\\...|....../............./","..-../.../......-.....\\........//.............|.........../...|.........../.......-...........................","......\\....../......./..|\\............/..........................\\..............\\.....\\.........|......-...|..",".|/.......\\.........|...../..//...................................-....-...........././-...............-......","........-....../......./..\\.............|.....................\\........../-....../........-...................","......-........./\\..................../..\\|..-//....|.\\--................|.....|.../............-............\\","........................|...-........|..............|..............|................./........................","...................|................|..../...-...................../...|......\\..................|...|\\.......","....-......../.\\...............\\-...\\...........................................................|.............",".............../.|.......|.........\\.\\.........................-|.............\\........-.../...-...||.........","....-...-\\.-............/...../...../..\\........\\|....\\...............\\.............-........................\\","........././.......//......-...|...........-...|..|....-......-.......|............................|../.......","\\.................|..../.....-..|......./.....................\\....../...|./\\.\\...............-............./.",".....-\\............../...................................../....\\.....-|...........-.\\...../............\\.....","......................./..\\..................|...............-.........-..../......../..|.....................","....\\...................../...../......|...........................\\......||.\\...-..............-...........|.","..........................|........../...........|....-.......\\........................-|..\\............/..|..","-./...|..............................................|./../.........-|.\\.|..........\\........../.........\\....","...........|......................-............................|.........................\\....-........../....","........................|/....\\......../.|.......\\.........../..................\\........................-....","...../.................-............................-.\\......|...............\\...........\\.....-.............-","....|-.............-.../....\\...........-...........|\\.............-.............../-....../..................","...............\\/.............../.|............................/.|...................-............-...........",".................\\.....\\........./......../.....\\.....|........-..\\..........................|................","...............-..-........-........../...../...\\.................\\..-.-.....|..........\\..............\\......",".............-....../....|.\\\\............|......|.........-.......|................/.......|./............\\...",".................................................\\.......\\-....\\.........|.....-..|.........-................/",".............../..............\\..........................\\.................-\\............-............-.......","/....|...../.....................................................-....\\.....-................../..........-..\\",".....\\.....................-...........|...\\.....-...../.......-.|.................|..........................",".............-..........-....-....-.-../....|.......-.\\.........................-........../..../.....\\.......","..........\\\\.........................../...-........./........\\............|.........../......................","............-......|..................................................................-......./...-...........","........\\............-......|....\\............................\\.\\........|......................../......\\...-","......../...|........|..-.-.../..../..........|........|....../....../../...........\\.............\\....../....","......|.........-........//........-../....\\........./..........|.-......-...-......-.............\\..|........","./......................../................-..........................................-.....--....|...........","....../............\\\\..../\\............|..-.|.|.-.............-........|................................./.../","..-......../.......|.......|........................-....-..............................-.....................",".........||............|.\\................|......................./.\\...............-..........|..../..-..\\...","\\|.......|..\\..........-.................../|.....-.............\\............./....................../........",".....................|....-.../...............\\.\\.............................................................","............|.......................\\...........-......|...././.-...../................|........-..........\\.\\","..........|../\\.|.......|\\..-............\\.-..............-....../...\\......||..\\........-.\\..../...\\......./|","..|.....................................-\\............//.......|..\\..............\\.........|....|.............",".....\\...............................-........./..........\\.........-....|..........-./../...\\................",".....\\./....-...\\....................\\|.........\\.............-...\\..........-...............|............./..","/./\\......\\.........\\........-../......../.....|/..................../.././..-......................|\\........",".|\\...-.\\\\.......................|........-\\.....|......|......./|..........-..../...................|.|......","...........\\.............\\./....|./.............................|.............................................",".../...../.|.................\\...../.....-..............|.\\.\\../.................../.............\\............","..............\\............-........-........\\....../............|....|...\\....\\...|.....-..../...............","/.\\.\\........-....\\.|....-....\\..../...-./...............-.......\\../........./..../........./.......\\........","|..............................-..\\............................\\......//........\\../...-../..\\......../.......",".............\\.../.\\............\\--............|................-|............|......../......................","...../....|................................-.....................................|./..........|.........\\.\\\\..","........./......-.....|............/.-.......\\..-.......................................|..................\\..",".......\\.\\.....................-|...../.....|........|............-..............-....../\\..\\....../..........","|..\\.|........../|..\\..|\\..-...................\\.................../.....-/...............|................../","...........|..-..............|........................-..............-...............-.............\\.........\\","...\\./.....-...|.-.......-................/.\\.....-..-...|......-.......................................--...|","...............-.........|............-............|./.......\\...../.....\\............-.\\.....................","..................|......|..........-..............................-....|............|.\\........./..-.........","|......................../.....-../|.....\\./..\\....|......................./........................./...\\....","....|............-........\\..-...............-................/\\..........................|........|.......\\-.","|......./.....\\.......\\..................-.\\..../....|............|.../..................../|.../........../..","...-|..................||......--......\\......|.............|\\........./.................\\......\\........\\....","./........./........-...................................\\......-......................................-.....-.",".....\\..-\\-....\\........................|........../.......|.........-.............................///........","./.............\\........./.................|.....................|..\\.........../...|...|..\\.|............-...",".................................\\.................................\\...........................-.........-....","...................\\....|...............|...............\\..../.....-.....................................\\....",".../.......-.......|...........|.....\\..................../......................|../.............\\...........","......-....................../..../.....-..............\\............./...........-.....\\....|...............|.","......-.......-.......|..................\\................-...|./.......--............../.......|.-.|.........","-..................\\........../..........................-...-.............../..............................|.","............/.........-..../\\..-.....\\....\\........./.........\\.........../.........|/.../....................","...................................../.....|..\\.............\\..........-.......\\............../.............|.","..../........./.........................-.......\\..|..|.....--|...../.............../..-/./.......|..../|.....",".........\\.|...|..|-...................................\\../..-.....|....\\....................-................","..../..-......|..............\\../............./..........-.|......../..........|.\\........|..................-","......|.................\\..........\\|...................-.../.\\.........../.|............../................./",".........................\\.................................../-./............................-..-.........|...","\\...........-................/....-.-....................................../...../..............\\.......-\\.../","-....................................\\.........................../.............................|..............","......................\\............\\.......\\..\\.............\\.................-....\\....-.|...................",".|-.-...../......./............./........|.................../.........-..\\........./.\\|...........-..........",".......\\..............\\.....-..........\\.|.\\...|.........\\/..........\\......|.\\...................-...........","../.-..................../..................|.....-...-....-............................-.-..-../..\\..........",".................................-.......|...../.\\.................\\..../................................-....",".-..|.....|.........-........|....-.........../...........\\......-..........................\\.................","..-.................../...|..-..|...................../...-...|........................................./|....",".........................-\\............................./....................../.......-................|.....","........../.......\\.........|...-/........./............./......................../...........-../..\\.-\\...-..","..........|....\\-........\\...................-.|....-.........\\................../.......\\....................","/...-..........\\......\\.........|.............|...........-....../.|..|..-............................|.......",".........\\.|........\\......\\....|.........................................../........-........................","....\\.....|.......\\....................................-............|.....-........./..\\...\\.....-..-.........",".|../...../.......|\\......\\............../....\\..|..-...-..............|-.................................-...",".|.\\......./........../........|..\\.../|......\\....|.........-.|.................-.........|............./..-.","...............-.................|...-../\\..../.......-.....|..\\./............................................","....-.|.........-........................../...................|...............\\........................../...","..|/...-..../../...-........-...-.....................................\\.......................................","............../....|.....-\\.....-................................../\\.....................\\....-........-.....","......../.|.................|......|.|.-....|...................../....................................\\......"]
inputTest = [".|...\\....","|.-.\\.....",".....|-...","........|.","..........",".........\\","..../.\\\\..",".-.-/..|..",".|....-|.\\","..//.|...."]
inputTest2 = [".|...\\....","|.-.\\....."]
inputTest3 =[".-.\\.",
         ".....",
         ".\\./."]

getNextDirs :: Direction -> Char -> [Direction]
getNextDirs d '.' = [d]

getNextDirs North '|' = [North]
getNextDirs South '|' = [South]
getNextDirs East '|' = [North,South]
getNextDirs West '|' = [North,South]

getNextDirs North '-' = [East,West]
getNextDirs South '-' = [East,West]
getNextDirs East '-' = [East]
getNextDirs West '-' = [West]

getNextDirs North '\\' = [West]
getNextDirs South '\\' = [East]
getNextDirs East '\\' = [South]
getNextDirs West '\\' = [North]

getNextDirs North '/' = [East]
getNextDirs South '/' = [West]
getNextDirs East '/' = [North]
getNextDirs West '/' = [South]

getNextDirs _ _ = error "missing pattern"

getNextCoordinate :: Map Coordinate Char -> Coordinate -> Direction -> Coordinate
getNextCoordinate m (x,y) toDir | toDir == North = (x,y-1)
                                | toDir == East = (x+1,y)
                                | toDir == South = (x,y+1)
                                | toDir == West = (x-1,y)
                                | otherwise = error "Unknown dir"

getNextBeams :: Map Coordinate Char -> Beam -> [Beam]
getNextBeams m (c,d) = filter (\(bc,_) -> Map.member bc m) beams
                    where
                        beams = map (\d -> (getNextCoordinate m c d,d)) nextDirs
                        i = m ! c
                        nextDirs = getNextDirs d i

getRowCoordinates :: String -> Int -> Map Coordinate Char
getRowCoordinates r y = fromList (zipWith (\x v -> ((x,y), v)) [0..length r] r)

getCoordinates :: [String] -> Int -> Map Coordinate Char
getCoordinates  [] _ = empty
getCoordinates (x:xs) y =  getRowCoordinates x y `union` getCoordinates xs (y+1)

getMap :: [String] -> Map Coordinate Char
getMap i = getCoordinates i 0

part1 :: [String] -> Int
part1 i =  length beam
        where
            m = getMap i
            beam = evolveBeam m (Set.fromList start) start
            start = [((0,0), East)]

part2 i = maximum scores
        where
            m = getMap i
            scores = map (\e -> length (evolveBeam m (Set.fromList [e]) [e])) entraces
            entraces = getEntrances i

getEntrances :: [String] -> [Beam]
getEntrances i = [((x,0), South) | x <- [0..w-1]] ++
                 [((w-1,y), West) | y <- [0..h-1]] ++
                 [((x,h-1), North) | x <- [0..w-1]] ++
                 [((0,y), East) | y <- [0..h-1]]
                where
                    w = length $ head i
                    h = length i 

evolveBeam :: Map Coordinate Char -> Set Beam -> [Beam] -> Set Coordinate
evolveBeam _ v [] = Set.fromList $ map fst $ Set.toList v
evolveBeam m v bs = evolveBeam m newVisited newBeams
                where
                    nextBeams = concatMap (getNextBeams m) bs
                    newBeams = filter (`Set.notMember` v) nextBeams
                    newVisited = v `Set.union` Set.fromList newBeams


printBoard :: (Int,Int) -> Map Coordinate Char -> IO ()
printBoard (w,h) m = mapM_ (putStrLn . (\yc -> Map.elems $ Map.filterWithKey (\(x,y) v ->y == yc) m)) [0..h-1]

getMapFromSet :: Set Coordinate -> Map Coordinate Char
getMapFromSet s = foldl (\m c -> Map.insert c '#' m) cleanMap (Set.toList s)
                where cleanMap = Map.fromList [((x,y),'.') | x <- [0..9], y <- [0..9]]
