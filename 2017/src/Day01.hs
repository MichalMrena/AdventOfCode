module Day01 where

import Data.Char ( ord )

solve :: Int -> String -> Int
solve len cs = sum $ zipWith check cs (drop len $ cycle cs)
  where check x y | x == y    = ord x - ord '0'
                  | otherwise = 0

part1 :: String -> Int
part1 = solve 1

part2 :: String -> Int
part2 cs = solve (length cs `div` 2) cs

solveDay :: IO ()
solveDay = do
  cs <- readFile "input/01_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ cs)
  putStrLn $ "Part 2: " ++ (show . part2 $ cs)