{-# LANGUAGE FlexibleContexts #-}
module Day06 where

import qualified Text.Parsec as P

fish :: Int -> (Int -> Int) -> Int -> Int
fish lastDay self currentDay
  | currentDay >= lastDay = 0
  | otherwise             = 1 + self (currentDay + 7) + self (currentDay + 9)

fishMemo :: Int -> [Int]
fishMemo lastDay = map (fish lastDay (fishMemoized lastDay)) [0..]

fishMemoized :: Int -> Int -> Int
fishMemoized lastDay = (fishMemo lastDay !!)

part1 :: [Int] -> Int
part1 ages = length ages + sum (map (fishMemoized 80) ages)

part2 :: [Int] -> Int
part2 ages = length ages + sum (map (fishMemoized  256) ages)

solveDay :: IO ()
solveDay = do
  ns <- readNums <$> readFile "input/06_day.txt" :: IO [Int]
  putStrLn $ "Part 1: " ++ (show . part1 $ ns)
  putStrLn $ "Part 2: " ++ (show . part2 $ ns)
  where readNums str = case P.parse nums "" str of
                         (Right ns) -> ns
                         (Left e)   -> error (show e)
        nums = map read <$> P.many1 P.digit `P.sepBy` P.char ','