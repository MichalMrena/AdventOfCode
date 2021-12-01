module Day03 where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

type Claim = [(Int, Int)]

parseInput :: String -> [Claim]
parseInput = map parseClaim . lines . map removeJunk
  where
    removeJunk c | c `elem` ['#', ',', '@', ':', 'x'] = ' '
                 | otherwise                          = c
    parseClaim s = [(x', y') | x' <- [x .. x + w - 1], y' <- [y .. y + h - 1]]
      where
        [_, x, y, w, h] = map read . words $ s

solvePart1 :: [Claim] -> Int
solvePart1 = length
           . filter (>= (2 :: Int))
           . map snd
           . M.toList
           . foldl (\m c -> M.insertWith (+) c 1 m) M.empty
           . concat

solvePart2 :: [Claim] -> Int
solvePart2 cs' = succ . fromJust . L.findIndex noIntersection $ cs
  where
    cs = map S.fromList cs'
    noIntersection c = (== 1)
                     . length
                     . filter (not . S.null)
                     . map (S.intersection c) $ cs

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/03_day.txt"
  putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
  putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)