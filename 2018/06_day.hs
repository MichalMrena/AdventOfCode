import Data.List
import Data.Function
import Data.Maybe
import qualified Data.IntMap as M

type Point = (Int, Int)
data Orientation = Clock | CounterClock | Colinear deriving (Show)

parseInput :: String -> [Point]
parseInput = map (read . ('(' : ) . (++ [')'])) . lines

manhattanDist :: Point -> Point -> Int
manhattanDist (px, py) (qx, qy) = abs (px - qx) + abs (py - qy)

solvePart1 :: [Point] -> Int
solvePart1 ps = res + 1
    where
        (lx, ly) = ((subtract 10) . fst . minimumBy (compare `on` fst) $ ps, (subtract 10) . snd . minimumBy (compare `on` snd) $ ps)
        (tx, ty) = ((10 + ) . fst . maximumBy (compare `on` fst) $ ps, (10 + ) . snd . maximumBy (compare `on` snd) $ ps)
        grid     = [(x, y) | x <- [lx .. tx], y <- [ly .. ty], not $ (x, y) `elem` ps]
        hull     = zip [lx .. tx] (repeat ly) ++ zip [lx .. tx] (repeat ty) ++ zip (repeat lx) [ly .. ty] ++ zip (repeat tx) [ly .. ty]
        hps      = map fromJust . filter isJust . map closest $ hull
        pid      = \(x, y) -> 1000 * x + y

        closest p = if d1 /= d2 then Just (pid p1) else Nothing
            where
                [(p1, d1), (_, d2)] = take 2 . sortOn snd . zip ps . map (manhattanDist p) $ ps

        freqMap = foldl inc M.empty grid
            where
                inc acc p = case closest p of
                                (Just cp) -> M.insertWith (+) cp 1 acc
                                Nothing   -> acc
        res = snd . maximumBy (compare `on` snd) . filter (not . (`elem` hps) . fst) . M.toList $ freqMap

solvePart2 :: [Point] -> Int
solvePart2 ps = res
    where
        (lx, ly) = (fst . minimumBy (compare `on` fst) $ ps, snd . minimumBy (compare `on` snd) $ ps)
        (tx, ty) = (fst . maximumBy (compare `on` fst) $ ps, snd . maximumBy (compare `on` snd) $ ps)
        grid     = [(x, y) | x <- [lx .. tx], y <- [ly .. ty]]
        res      = length . filter ((< 10000) . totalDist) $ grid
            where
                totalDist p = sum . map (manhattanDist p) $ ps

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/06_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)