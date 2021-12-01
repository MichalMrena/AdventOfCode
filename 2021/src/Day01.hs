module Day01 where

import Data.List ( tails )

part1 :: [Int] -> Int
part1 xs = length . filter (uncurry (<)) . zip xs $ tail xs

part2 :: [Int] -> Int
part2 = part1 . map sum' . tails
  where sum' (a : b : c : _) = a + b + c
        sum' _               = 0

solveDay :: IO ()
solveDay = do
  xs <- map read . lines <$> readFile "input/01_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)