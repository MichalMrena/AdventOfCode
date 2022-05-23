module Day07 where

import Data.List ( elemIndex )

type LossFunction = (Double -> Double -> Double)

minimize :: LossFunction -> [Double] -> Double
minimize loss xs = minimum es
  where ms = [minimum xs .. maximum xs]
        es = map (sum . map abs . zipWith loss xs . repeat) ms

part1 :: [Double] -> Double
part1 = minimize (\x m -> abs (x - m))

part2 :: [Double] -> Double
part2 = minimize (\x m -> let d = abs (x - m) in (d + 1) * (d / 2))

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/07_day.txt" :: IO [Double]
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where parseInput = read . (++ "]") . ('[' :)