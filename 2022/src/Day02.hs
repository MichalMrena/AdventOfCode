module Day02 where

import Data.List ( sortOn )
import Data.List.Split ( splitOn )

part1 :: [(Char, Char)] -> Int
part1 = sum . map score
  where score p = roundScore p + charScore (snd p)

        charScore 'X' = 1
        charScore 'Y' = 2
        charScore 'Z' = 3

        roundScore ('A', 'Y') = 6
        roundScore ('A', 'Z') = 0
        roundScore ('B', 'X') = 0
        roundScore ('B', 'Z') = 6
        roundScore ('C', 'X') = 6
        roundScore ('C', 'Y') = 0
        roundScore _          = 3

part2 :: [(Char, Char)] -> Int
part2 = sum . map score
  where score (a, 'X') = 0 + charScore (lose a)
        score (a, 'Y') = 3 + charScore (draw a)
        score (a, 'Z') = 6 + charScore (win a)

        lose 'A' = 'C'
        lose 'B' = 'A'
        lose 'C' = 'B'

        draw a = a

        win 'A' = 'B'
        win 'B' = 'C'
        win 'C' = 'A'

        charScore 'A' = 1
        charScore 'B' = 2
        charScore 'C' = 3

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/02_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where parseInput :: String -> [(Char, Char)]
        parseInput = map parseLine . lines
        parseLine (a : _ : b : _)= (a, b)
        parseLine _ = undefined