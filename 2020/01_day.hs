import Data.Bool
import Data.IntSet (IntSet, member)
import qualified Data.IntSet as S

parseInput :: String -> IntSet
parseInput = S.fromList . map read . lines

solvePart1 :: Int -> IntSet -> Int
solvePart1 target xs = S.foldl' check 1 $ xs
    where
        check acc x = bool acc (acc * x) $ (target - x) `member` xs

solvePart2 :: IntSet -> Int
solvePart2 xs = S.foldl' check 1 $ xs
    where
            check acc x = let p1 = solvePart1 (2020 - x) xs
                          in bool acc (x * p1) (1 /= p1)

solve :: IO ()
solve = do
    xs <- parseInput <$> readFile "./input/01_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 2020 $ xs)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ xs)