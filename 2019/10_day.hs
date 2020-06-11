import Data.List
import Data.Function

input  = "....#...####.#.#...........#........#####..#.#.#......#####...#.#...#...##.##..#.#.#.....#.....##.#.#..#.......#..#...#.##........#..#.......#.##...##...###...###..#...#.....#.....##.......#.....#.........#.#....#.#...#...#.##.##.....#....##..#......#...###..##..#..#...#......##...#....###..##.....#...#.#...#......#.#.#..#...###....#..#.#......#...#.......#.#....#...##.......#..#.......#..#...#...........#.....#.....#.#...#.##.####..#....####..#.###...#....#..#...##....#.#..#.#......##.......#....#...#.#....#.#.#..#...#.##.##..#.........#.....#......#.#.#.##.....#..###...#.#.###.......#..#.#....##.....#...#.#.#...#..#.#..##.#..........#...#.....#.#.#...#..#..#...###.#...#.#..#..#..#.....#.##..##...##.#.....#.......##....#.##...#..........#.##.......#....###.#...##........##.##..##.#..#....#......#......###...........##...#..#.##.##..##....#..#..##..#.#.#....#..##.....#.#............##....###.........#....#.##.#..#.#..#.#..#...#..#...#.#.#.....#....#......####...........##.#....#.##......#.#..#....#...#..#...#.####...#.#..#.##.........####.....#..#....#....#....#.#.##.#..###..####...#.......#.#....#.#.###....#....#..........#.....###.#...#......#....##...##..#..#...###....#...###.###.........#.#..#.#..#....#.#.............#.#....#.............#...#.###...##....##.#.#.#....#.#."
width  = 36
height = 36

type Point = (Int, Int)

pointOp :: (Int -> Int -> Int) -> Point -> Point -> Point
pointOp op (x1, y1) (x2, y2) = (x1 `op` x2, y1 `op` y2)

isInGrid :: Int -> Int -> Point -> Bool
isInGrid w h (x, y) = x >= 0 && y >= 0 && x < w && y < h

indexToPoint :: Int -> Int -> Point
indexToPoint w i = (i `mod` w, i `div` w)

rockPositions :: String -> [Point]
rockPositions s = map (indexToPoint width) $ map snd $ filter (('#'==) . fst) $ zip s [0..(length s)]

shadowedPositions :: Int -> Int -> Point -> Point -> [Point]
shadowedPositions w h from to | from == to = []
                              | otherwise  = takeWhile (isInGrid width height) [pointOp (+) to $ pointOp (*) (i, i) normVector | i <- [1..]]
    where
        vector     = pointOp (-) to from
        n          = uncurry gcd $ vector
        normVector = pointOp div vector (n, n)

visibleRocksFrom :: [Point] -> Point -> [Point]
visibleRocksFrom pts pt = let shadowedPts = nub $ concat $ map (shadowedPositions width height pt) pts 
                          in  pts \\ shadowedPts
             
angle :: Point -> Double
angle p@(x, y) | x >= 0 = atan2 (fromIntegral x) (fromIntegral y)
               | x <  0 = 2 * pi + (atan2 (fromIntegral x) (fromIntegral y))

vaporize :: Point -> [Point] -> [Point] -> [Point]
vaporize center []  result = result
vaporize center pts result = let visible = visibleRocksFrom pts center 
                                 toAngle = (\ p -> angle $ pointOp (*) (1,-1) $ pointOp (-) p center)
                                 sorted  = sortBy (compare `on` toAngle) visible 
                             in  vaporize center (pts \\ visible) (result ++ sorted)

solvePart1 :: Int
solvePart1 = let rocks = rockPositions input
             in  subtract 1 $ maximum $ map length $ map (visibleRocksFrom rocks) rocks

solvePart1ForPart2 :: (Int, Int)
solvePart1ForPart2 = let rocks = rockPositions input
                     in  maximumBy (compare `on` (length . (visibleRocksFrom rocks))) rocks

solvePart2 :: Int
solvePart2 = let center = solvePart1ForPart2
                 rocks  = filter (/=center) $ rockPositions input
             in  (\ (x, y) -> 100 * x + y) $ (vaporize (25,31) rocks []) !! 199