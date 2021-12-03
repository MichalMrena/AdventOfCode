{-# LANGUAGE TupleSections #-}
module Day03 where

import Data.Char ( ord )
import Data.List ( transpose )

bitFreq :: [Char] -> (Int, Int)
bitFreq = foldl incFreq (0, 0)
  where incFreq (z, o) '0' = (z + 1, o)
        incFreq (z, o) '1' = (z, o + 1)

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
part2 nums = oxygen * co2
  where bitOxygen bs = let (z, o) = bitFreq bs
                       in if o >= z then '1' else '0'
        bitCo2 bs = let (z, o) = bitFreq bs
                    in if z <= o then '0' else '1'

        oxygenBits = go bitOxygen (map ([],) nums)
        co2Bits    = go bitCo2 (map ([],) nums)

        oxygen = sum $ zipWith (*) oxygenBits (iterate (*2) 1)
        co2    = sum $ zipWith (*) co2Bits (iterate (*2) 1)

        go :: ([Char] -> Char) -> [(String, String)] -> [Int]
        go sigBit [(ls, rs)] = map (subtract (ord '0') . ord) $ reverse rs ++ ls
        go sigBit ns         = go sigBit ns'
          where bit  = sigBit $ map (head . snd) ns
                rest = filter (\(_, r : rs) -> r == bit) ns
                ns'  = map (\(ls, r : rs) -> (r : ls, rs)) rest

solveDay :: IO ()
solveDay = do
  numbers <- lines <$> readFile "input/03_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ numbers)
  putStrLn $ "Part 2: " ++ (show . part2 $ numbers)
  putStrLn "done"