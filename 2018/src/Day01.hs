module Day01 where

import qualified Data.IntSet as S

parseInput :: String -> [Int]
parseInput = map read . lines . filter (/= '+')

solvePart1 :: [Int] -> Int
solvePart1 = sum

solvePart2 :: [Int] -> Int
solvePart2 = loop S.empty . scanl (+) 0 . cycle
  where
    loop fs (x : xs)
      | x `S.member` fs = x
      | otherwise       = loop (S.insert x fs) xs
    loop _ _ = undefined

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/01_day.txt"
  putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
  putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)