module Day01 where

import Data.List ( sortOn )
import Data.List.Split ( splitOn )

part1 :: [[Int]] -> Int
part1 = maximum . map sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . sortOn negate . map sum

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/01_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where parseInput :: String -> [[Int]]
        parseInput = map (map read) . splitOn [""] . lines