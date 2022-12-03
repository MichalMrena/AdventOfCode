module Day03 where

import           Data.Char ( isLower, ord )
import           Data.List ( sortOn, foldl1 )
import           Data.List.Split ( chunksOf )
import qualified Data.Set as S

splitInHalf :: String -> (String, String)
splitInHalf s = go "" s s
  where go first slow     []         = (first, slow)
        go first (s:slow) (_:_:fast) = go (s:first) slow fast

itemPrio :: Char -> Int
itemPrio c | isLower c = 1 + (ord c - ord 'a')
           | otherwise = 27 + (ord c - ord 'A')

part1 :: [String] -> Int
part1 = sum . map bagPrio
  where bagPrio bag = sum (S.map itemPrio items)
          where (first, second) = splitInHalf bag
                items = S.intersection (S.fromList first) (S.fromList second)

part2 :: [String] -> Int
part2 = sum . map grpPrio . chunksOf 3
  where grpPrio = sum . S.map itemPrio . foldl1 S.intersection . map S.fromList

solveDay :: IO ()
solveDay = do
  xs <- lines <$> readFile "input/03_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)