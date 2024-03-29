module Day14 where
import qualified Data.Map as Map
import Data.Map (Map, empty, union, fromList, keys, insert, (!), filter, member)
import Data.Maybe (isJust, isNothing, fromJust)

inputTest = ["OOOO.#.O..","OO..#....#","OO..O##..O","O..#.OO...","........#.","..#....#.#","..O..#.O.O","..O.......","#....###..","#....#...."]
input = ["#..#.#.O...OO....##O....#....O.O#.......O.O.#.O..#..........O...#OO.O..OO..#.#..O.O.OO...#.OOO...OO.","..O..O#OO#....#.#O.#......#...O...O..O##.....O..........OOO.#......#OOO....#.#..O..O......OO....O.OO","O..#O..#O#...O#....O#.O...#.#..##O.O#OOO..OOOOOO.O.#O.O....O......OO#O#..O.........O.#....O..O......","O.O.....#O##..OO.........#.........O.#...#..#...OO##......O.....O..OO#.#.##..OO.........O#.#....#.#.","......#....#OOOO..O#O...#...O#....O#..#..OO##..###.#.O#.O...#O.#..........O..##....O#O....OO.....O##","..##.#O...#......#.O...O.#O.O....#....#..##.........#.O#O.O.#.####..O#.O#......O...#.#.#O..#O...OO#.",".OO.O....O#.O..O.......O.OO#O....#..O.#....OO##..O.O....O...O.#....#......O....#.O..OO....OO..#....#","..OOO......#..#.....#.O.........O....OO..##.OO##...#.#.O#.#......#....###...#..#...##....#..O#...O..","O#.O.O#.O......#..O..O.OO##...O.....O...##...O.OO.O..##....#.O......O#.....O.OO.O..O.........#..#..O",".##..#.....#....#...O.O#O#...##O....O..O.#O#.#OO.#..O#..O..#O###...........#.##O.###O...O.#....O#..O",".#.#.#O...OO....O.###........OOO...#.#.O.....O.....#..#....#...O....O.#O...O.O.#.#...#O#O#O.#O.O...O",".#......O.O.#..O#..#......#.....O...#.O.....O.......#.......#..O...#.#.#.##.#.#...O...........###O.#","#..O..#....##O##..#.#.OO..O#O........O#..##O....#OOO..O.#.#.#..##.OO.#..O.#...O..O.O......#..O......","O#O.#...O...O....#....O..O#...O#..#O.....#..#....#.O.O##.#.O.O#O#......#.##..O.O#.O.....O...O#O..OO#",".OO#.....O....O.##...#.......#O#..OO.OO#.#..O.#.O.O..O.#.O.O..OO.O...OOO...#O..........#..#.O.O#...O","O..O.#......#.O.O....#..O.....O...#....O..##.#.###....#.#...#O....O........O...O...OO...##..O.OO#..#",".O.O.####.#.O....O#..O.O#.##.OO..#.....O......OO..#.O.O...#..#....#..O..O..O#O......#O.OO...##O...O.",".O.....O.........#...O..O..O.O#..##.#...O#.......O.##O.....#.#.#..O..O...#.OO#.#.O..#...O..O.O......","......O.O...O......O#.O.#OO.....#..#..........O..OOO...O...#.#..O#....O.#.O..O.#O..O..#.#..#.O..#...","##.#.#O...O.#O..O..#.OO..###.......O....OO......#...O...O.#...O..O.#...#...O.O#..##O#..#...O.#....OO",".OO#..#.#.#O..O.O..O#....#.O......###.O.O.O.#.......##...O##OO#.....##..O#.#..#.#.O.#.#...O..O.#...#","#...OO..O..OO......O..#..#...O....O.OOOO##.....#O.....#.#..#..##.......O..O.#...#.#..#.OO#...O.O..O.","##...O.OOO#.....#..O#..OO......OO#O.#O.#....#.OOO..#..#OO.O.O..#.O......OO.............#.O#.OO..#OO.",".O#.#.....O....O#...#...O....O#O#........#O#.O.#.O...O.O..#O...O......#..O.#............#...##O#...O","#...O.......#.O.O..##.O.#....O..#.....O.O...#...O.O..O.....O.OO..O..##..O...O..O......O...OO.OO.O.O.","O......#......#OO...OO#.#O#....#....#.O.OO.#...O.#.#..O..#O..#....O..O....#....#....##.....O.OO#.#O.","...O.O..#..#.O.#.....##.O.O.#...O..O..#.....#..OO#...O..###..#.....#.#O.....O.....##.#....O..O...O##","#..#O#O.OO.#...O#..##.#..O.O.#....O#.#OOO..#O...OO....O.O#O.O..O#..#..O#.#.#O.##.#O.#O..O...#...#OO.",".....O.....OO.....O.O#..OO...O.......#O....O....OO.O#.......#.O.#.O.O#O.#.....#....O.O.#..#.....O.OO","...OOOO....##.O.....OOO.O..OO...O#..O...#O....O..O#....OO...#.#....O#.O.#......#O.O....O.......O..O.","...#.##.##O#O#....O....#.#...#........#...#.O..#.#O.....####.O#....#.O#......O...#....#.O..#..O.#.O.","..O.......O..O.###.....#......##.#.##.OO..OO........OOO..#.O..O.....O...#O....##..O.OOOO......O#.O..",".#..O.O.O..#.#.#..O##O.#.......O..#....O.O....#....#.#.#O..##.....O.#O.....##O.#..#..O..#..OO....#.#","...O.....O#..O#O#...O....##..O.....O..#.O...#..O..#....O#O...OO..OO.#........O............#....O...O",".##OOO###..O..OO..##O#....O....OO....#.O#..#O....#.##O......O.#O.#O..O......#......#.##O#OO......O#.",".#O#.....O#.....#...##...O..#O..O.#.O..#..O........O#.....#..O..OO#O...OO......O..#...O#...#.OOOO...",".........O.#.#O....#OO...##........#.#..#.#..........OO..O..O..OOO#.........O.....O#..#OO.....#.#..#","#..O.O...#..O.O##OO#..O.#O.....O.O..O.O.O.#.O.#....#.......O#O#.OO..##.....OO...#.........O.O....O.O","OO#.#.##...#....#........OO....O..OO..O#.####.....#OO..#.....#........OO.#..#O..#.O#O#O.O..OO#O.O.O#","#....O.O#..#O...OO..O.OOO.#.O.##.##..O..#O#.....O.....#O.O#.#...#...O.O.........O..#...#....O...O.#.","....#O..OO..O......#O#....O.O....#..#....#....#.....OOO##.O..#..####..##.#..##..O...O.....O#....OOO.","O...O..O#OO.####O..#OO..#.#.O....O.#.O.##........#..OOO...O....O........O.O.O.O.OOO..#OOO.#.#OO..#..","O#O#.#..#..O..#.....#.#......O..O...O.O...#..O..#.O.....#O.....#.......#.O.O.......O...#....O..OOO#O",".#...O...#.O......O....O....O.##.OO...O......#..O#O......O..O.........O.O...#.....#...#.##......#O..","##O#.O......#.O..O..O.OO..##OO##.#O..O.#OO...O#..O.O.O..#.OO...O#O##O......#.#.#....O..O.......O...#","O.#O.OO##....OO.......O......#....#O##....#....O#O......O..#.#.....O.OO.....O.#..O.#OO.O..OO.#..#.#.","O.#.#.....#.O#.........O....O.#.O.#..O....O....OO.O#.#...O.O#..OO#O...#..#..#....#.##..OO.....#.....",".#O.#O...O..#.OO#..O....#....#.#####............OO.O..OO.#....OO......#O...O.....O.#O..#.....#...O.O","#.#....OO.O.O...O....O......#...#....#...##..#.#.....O..O#.#O.#.OO.##.O.#..........OO.O#.......##O#.","O.O..O..##...#..........O.#.#.....O.#...O#.O..O........#.O#O......OO.#....OO..O.....O.O......#.OO..O","#...#O..OO...#O#.OO#.O..OO.O.O.O##.O#O##......O#..O.#O...##...OOO...#O...O...O..O.O.##.#.......##..O",".#.O#O........###O.##.O#..#OO.O.....OO......O......O.O#.....#O#...#...#.O......O.#.....#.O.####.O...","..#..OOO...O#.###OOO#.#.O#....##O..#O.##......O.##...O...#...O....O.....#.O.#....#.......O..##...O.#","OOO.##.#..OOO.....O...#..##.O...#...#.O##.......#.OO#..#.#...O.....#.#...##.O.O.O....#.O....O#..O...","#..#..OO.....#O.....O#..O.O......O.###O.#..#..OO.#.O.#...OO...#..O.#O#..#.#..O.O..O....OO#...O..#...",".#.O..#...O.#.O.#.OO..O..#.O..OOO.O......O.#.#O.......OO...O#.......O.O.....#..#.......O...O.O..##O#",".O#....#O.#.....#......O...O........O...O.O........OO.O#.O...#.#....O..#.#O...#.OO#..#OO#.....O...##","........O..#..#.O###O..O..O.O...###.O...#..O..O.O#O...#..O#O#....O.#.....O..#..OO.O.O...O.......#OO.","#.OO..O..O..#.O..#..O....O.#O#.OO.O#.#...#...........O..O..O#.O....#......#........O....O.O.O.#..O..",".O........#......#O...#O#...##O##...O.....#.O...##O.#.#O....O#...#O....#OO..#.#.#...#..#O.#......O.O","O#..O..#O...O..O.........O......#.OO.O#.O..O....O........#.O..#...OOO.#..O.##O....O..O#.#..#O#..O##O",".#..#.....#....#..O.O..O..#..OO##O..####.............O.#OO....#.O.....#OO...OO..#O#.#.#..OO.........","..#O..#...O.O....OO.O#.#O.##...#....#OOO...O.....#......#....O##OO....#....#....#O#.O.#.#..O...#..O.","...OO#..#..O....O.OO.#O##..O.....#..#....##.O.#.O....O....O.OO#.#O.OO.OO.#O.........#..##....O..O...",".O#.........O.....#.OO.OO.O.O..O.O...#.O...O.....O..OO..#OO....O.OO..#.#O.O.#..OO..O....O.O..O#O..OO","##.#..O#....O...#...O....#.......#.#..#..O...#O##...O#...O...#..........O.#O....OO..........#.......",".OO...O.#............O.#..#..O...#.#...#.O##OO#...OOO..O..#.OO..#.......#OO.............O.#..#O.#...","...#.#O.#....#.##..........#...#OO.O..#.O.....O.O..#.#O.....#..#O............OO#O..#O.....O...OO..O#","#O#.O.....O.......#.......OO#..O.......O#...O........O..#.#.........OO..#....#....O...O.....#..O.#..","....O..O.##..#.....O....##..OOO....O.O....O.....OO#O.O#.....##O.##.O.O.#..OO.OO.##.O.#........OO....",".O.O#...#OO.#..O.#..#.##.#.......O#O.O......O.O......OO..O....#.O........#.#....#.O#O.#..O#O......O.",".OO..#.O.#.....O#O..#..#.#....O.O####.#.O.O#OO......O........#O.O#OO#O..O.O......O.O.O#...#.O..O...O","....O..O...##...##.##.O...#......#.O..O#O...O#O..OO....OO.#..#O#.OOO.#..#..O....#OO....#...O.#O..OO.","#..O#..#..#..O.O.O..#..O.O#.O.....OO....O...O.OO#OO....O.#O..O.O......O..OOO...O#OO..OO.O#....O.OO..","O.O.....O...##..#...#.O.#.#..O..O................O.#..OO.#....#......O#O..O.#.O.#..#O...........O..#",".....OO..O..OO...O.#..........O.#O.........OO#.............#..OO...##.#.#.OO.O.O.#.##.#.OO.OOO......","...#...#OOO.OOO..OO..##O.O.......O#.#O....O...#.##.##.##.#.O..O.O#O....O.O...O.#.O...O........O..O..",".O.....#.O#..#..##.OO......#........#...O.#O.#..O#O.O.OO...##......O..O.....#..O...O.O.OO.O.O.#O##.O","O....O.O..#.#...#..O.#O...#O.O..OOO.....O.#..O.O.#..#...#..O.....#..#..##...........###.....#...OO.#","..#.....O.OO...O#........O..#O.....O.....OO...#..#.OOO......O#.#.O.O.##O.#..........O.#..O..OOO.....","#..O.#O#.#.......#..O..#..#...#.O..#.....O........O.#..#OOO...O......#..O...O..#....#..O.........#..","..O.OO..#O....OOOO...O....O#O....##OO.O.O..O.O......O..O.OO....O.OOO.O......#..O...#O....O...#.OO#..","O.#O....OO....##.O......O...O..O.#....O#OO.O..#....O....##O..O....O..#.O...#...OO.O..O.O#.....O.OO..","#.#O....O..#.O#..O...O#...O.O..O..........OO.......O#......O.......##.#..#.###.#O.O.......OO..#.O#O.","O..##...O#O...........OO..#.#O...#OO......O#OO.#OO.#..O#OO#.#O....OO.##..O.#O.#..#.O.#.#..#O.OO.O.OO","O.####..O#..O#....#.O#.O.#......O..O..##...#........O.......OO##.O.#.....O..O#..O...#..OO.....#....O","...####..O..O#.#..OOO.....OO..............O.#OO.O#..##OO...#....O..#.##........##.O....O.....#..#..O","..O.O..#OO#..O...O...OO...O....#...O.OO#.....#OO.....#.O......O.#O.#.#..#..........###OO#.OO...O..O.",".O#.....O...O......O.#..OO..OOO......#O#.O....O.....O..O##.O#....OO.O....O..#.....O.O.O..O....OO.O..","....OO.....##O....O##..#..OO....O.#O.OO...#....###.O....O.##..#..OOO#O#.......O...O.O..#O.......O..#","...O...O..O.......O.#O.O........#.OOOO..............O#...O.O#O.....#......O#.#.........##.#.##O.#...",".O.....O.O.O.#.O#OO#.....#..O.O#.O#.....##.....O..............#.......O...#O.#O...#.O...#.#O.....#O.",".........##...#.O.#..#...#OOOO.#.OOOO...#O....O...O#....OO..##.OO..O.#.....##.O.#.O#...OOO..#.O##.O.","#O..#....#.O....OO..OO#...#..#..O......#...#O.O....#....O...O.#.......OO....O...#...#...#.#...O....O",".OO.#O.OOO##OO..#OO#..O.O..O.#..#..#....O...OO.O..##..#.....OO.#...#.....#.#....O.O..#...O.#......#.","OO..#..O#O.O..O.#..............##..OO#O#O.##..O#...O..O#...O....##..##O#............###OO........##.","..O..#..#.O....O.#.........OO.#...O.....OO.#..O..#....O...#..#...##..OOO....#..O#....#.......O.#O.O#","O.O.OO..O.#.O.O.#.##.##.O..O.O...###..O..##..O#O#....O....O..O#O.........##..O#.OO.O....#O.OOO...#.#","..#....O..O.....OO..#..O.OO..#OO.OOO...OO..##.O..O......O#.O.#..#O.O#..#.....O...O....#..###.O....#.","#.......O.O.O.#....#..#O.O...OOO.......O...........#.O..O..O.O..#..#.O...O.O..O....#.O.O#....OO.O..#"]

inputTest2 = ["..", "O."]
type Coordinate = (Int,Int)


tilt :: Char -> [Coordinate] -> Map Coordinate Char -> Map Coordinate Char
tilt _ [] m = m
tilt d (c:xs) m = tilt d xs newM
                    where newM = move m d c

move :: Map Coordinate Char -> Char -> Coordinate -> Map Coordinate Char
move m d c@(x,y)   | m ! c /= 'O' = m
                    | isJust nc = insert (fromJust nc) 'O' (insert c '.' m)
                    | otherwise = m
                        where
                            nc = getFurtherstFreeCoordinate d sc m
                            sc = getNextCoordinate c d

getNextCoordinate :: Coordinate -> Char -> Coordinate
getNextCoordinate (x,y) 'N' = (x, y-1)
getNextCoordinate (x,y) 'E' = (x+1, y)
getNextCoordinate (x,y) 'S' = (x, y+1)
getNextCoordinate (x,y) 'W' = (x-1, y)
getNextCoordinate (x,y) _ = error "error"

getFurtherstFreeCoordinate :: Char -> Coordinate -> Map Coordinate Char -> Maybe Coordinate
getFurtherstFreeCoordinate d c@(x,y) m  | not (member c m) = Nothing
                                        | p == 'O' || p == '#' = Nothing
                                        | isJust rec = rec
                                        | isNothing rec = Just c
                                        | otherwise = error "Error"
                                            where
                                                p = m ! c
                                                rec =  getFurtherstFreeCoordinate d sc m
                                                sc = getNextCoordinate c d

getScore :: Int -> Map Coordinate Char -> Int
getScore height m = sum $ map (height -) ys
                    where ys = map snd $ keys $ Map.filter (=='O') m

part1 :: [String] -> Int
part1 i = getScore y $ tilt 'N' cs m
        where
            m = getMap i
            cs = [(x,y) | x <- [0..length (head i) - 1], y <- [0..length i - 1]]
            y = length i

cycleTilts :: (Int,Int) -> Map Coordinate Char -> Map Coordinate Char
cycleTilts (w,h) m = foldl (\m (d,cs)  -> tilt d cs m) m [('N',csN), ('W', csW), ('S', csS), ('E', csE)]
                where
                    csN = [(x,y) | x <- [0..w - 1], y <- [0..h - 1]]
                    csW = [(x,y) | y <- [0..h - 1], x <- [0..w - 1]]
                    csS = [(x,y) | x <- [0..w - 1], y <- reverse [0..h - 1]]
                    csE = [(x,y) | y <- [0..h - 1], x <- reverse [0..w - 1]]
part2 i = map (getScore (snd d)) cycles
        where
            cycles = loop 1000 d m
            d = (length (head i), length i)           
            m = getMap i


printBoard :: (Int,Int) -> Map Coordinate Char -> IO ()
printBoard (w,h) m = mapM_ (putStrLn . (\yc -> Map.elems $ Map.filterWithKey (\(x,y) v ->y == yc) m)) [0..h-1]

getRowCoordinates :: String -> Int -> Map Coordinate Char
getRowCoordinates r y = fromList (zipWith (\x v -> ((x,y), v)) [0..length r] r)

getCoordinates :: [String] -> Int -> Map Coordinate Char
getCoordinates  [] _ = empty
getCoordinates (x:xs) y =  getRowCoordinates x y `union` getCoordinates xs (y+1)

getMap :: [String] -> Map Coordinate Char
getMap i = getCoordinates i 0

loop :: Int -> (Int,Int) -> Map Coordinate Char -> [Map Coordinate Char]
loop 0 _ m = []
loop i d m = newCycle : loop (i-1) d newCycle
                        where newCycle = cycleTilts d m