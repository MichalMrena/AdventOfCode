import Data.Set (Set, (\\))
import qualified Data.Set as S

type Point2 = (Int, Int)
type Point3 = (Int, Int, Int)
type Point4 = (Int, Int, Int, Int)

parseInput :: String -> [Point2]
parseInput s = map (parsePoint . fst)
             . filter ((== '#') . snd)
             . zip [0 .. ]
             . filter (/= '\n') $ s
    where
        colCount   = length . head . lines $ s
        parsePoint = \p -> (p `mod` colCount, p `div` colCount)

neighbours3 :: Point3 -> Set Point3
neighbours3 p = S.fromList . map (plus3 p) $ ds
    where
        plus3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
        ds = filter (/= (0, 0, 0)) $ [(x, y, z) | x <- [-1, 0, 1]
                                                , y <- [-1, 0, 1]
                                                , z <- [-1, 0, 1]]

neighbours4 :: Point4 -> Set Point4
neighbours4 p = S.fromList . map (plus4 p) $ ds
    where
        plus4 (x1, y1, z1, q1) (x2, y2, z2, q2) = (x1 + x2, y1 + y2, z1 + z2, q1 + q2)
        ds = filter (/= (0, 0, 0, 0)) $ [(x, y, z, q) | x <- [-1, 0, 1]
                                                      , y <- [-1, 0, 1]
                                                      , z <- [-1, 0, 1]
                                                      , q <- [-1, 0, 1]]

run :: (Ord a) => (a -> Set a) -> Set a -> Int
run neighbours ps = S.size . (!! 6) . iterate evalCells $ ps
    where
        activeNeiCount actives = S.size . S.intersection actives . neighbours
        stayActive actives     = (`elem` [2, 3]) . activeNeiCount actives
        goActive actives       = (== 3) . activeNeiCount actives
        evalCells actives      = S.union oldActives newActives
            where
                inactives  = (\\ actives) . S.foldl S.union S.empty . S.map neighbours $ actives
                oldActives = S.filter (stayActive actives) actives
                newActives = S.filter (goActive actives) inactives

solvePart1 :: [Point2] -> Int
solvePart1 = run neighbours3 . S.fromList . map (\(a, b) -> (a, b, 0))

solvePart2 :: [Point2] -> Int
solvePart2 = run neighbours4 . S.fromList . map (\(a, b) -> (a, b, 0, 0))

main :: IO ()
main = do
    ps <- parseInput <$> readFile "./input/17_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ ps)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ ps)