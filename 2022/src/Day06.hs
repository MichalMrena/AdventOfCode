module Day06 where

import Data.List ( tails, nub )

solve :: Int -> String -> Int
solve n = (+n) . length . takeWhile ((/=n) . length . nub)
        . map (take n) . tails

part1 :: String -> Int
part1 = solve 4

part2 :: String -> Int
part2 = solve 14

solveDay :: IO ()
solveDay = do
  xs <- readFile "input/06_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)