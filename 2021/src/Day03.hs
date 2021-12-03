module Day03 where

import Data.Char ( ord )
import Data.List ( transpose )

part1 :: [[Char]] -> Int
part1 nums = gamma * epsilon
  where freq  = map (foldl incFreq (0, 0)) . transpose $ nums
        incFreq (z, o) '0' = (z + 1, o)
        incFreq (z, o) '1' = (z, o + 1)
        gammaBits   = reverse $ map (\(z, o) -> if z < o then 1 else 0) freq
        epsilonBits = map (1-) gammaBits
        gamma       = sum $ zipWith (*) gammaBits (iterate (*2) 1)
        epsilon     = sum $ zipWith (*) epsilonBits (iterate (*2) 1)

part2 :: [[Char]] -> Int
part2 nums = 0
  where 
        go :: ([Char] -> Char) -> [(String, String)] -> String
        go sigBit [(ls, rs)] = reverse rs ++ ls
        go sigBit ns         = go sigBit (filter (\(_, r : rs) -> r == bit) ns)
          where bit = sigBit $ map (head . snd) ns

solveDay :: IO ()
solveDay = do
  numbers <- lines <$> readFile "input/03_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ numbers)
  -- putStrLn $ "Part 2: " ++ (show . part2 $ numbers)
  print (part2 numbers)
  putStrLn "done"