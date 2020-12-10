import Data.List
import Data.IntMap ((!))
import qualified Data.IntMap as M

parseInput :: String -> [Int]
parseInput = sort . map read . lines

solvePart1 :: [Int] -> Int
solvePart1 xs = let pairs  = zip xs (tail xs)
                    diffs  = map (uncurry subtract) pairs
                    count1 = length . filter (== 1) $ diffs
                    count3 = length . filter (== 3) $ diffs
                in (count1 + 1) * (count3 + 1)

solvePart2 :: [Int] -> Int
solvePart2 xs' = let xs      = reverse (0 : xs')
                     initMap = M.insert (3 + head xs) 1 M.empty
                 in (foldl' process initMap xs) ! 0
    where
        process m x = let x1 = M.findWithDefault 0 (x + 1) m
                          x2 = M.findWithDefault 0 (x + 2) m
                          x3 = M.findWithDefault 0 (x + 3) m
                      in M.insert x (x1 + x2 + x3) m

solve :: IO ()
solve = do
    xs <- parseInput <$> readFile "./input/10_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ xs)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ xs)