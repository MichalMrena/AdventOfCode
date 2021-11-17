module Day14 where

import           Data.Sequence ( (><), (|>) )
import qualified Data.Sequence as S

scores :: [Int]
scores = 3 : 7 : go 0 1 (S.fromList [3, 7])
  where
    go i1 i2 xs = ds ++ go i1' i2' xs'
      where
        n'  = S.length xs + length ds
        s1  = xs `S.index` i1
        s2  = xs `S.index` i2
        i1' = (i1 + s1 + 1) `mod` n'
        i2' = (i2 + s2 + 1) `mod` n'
        (ds, xs') = case (s1 + s2) `divMod` 10 of
                      (0, x) -> ([x],    xs |> x)
                      (x, y) -> ([x, y], xs |> x |> y)

part1 :: Int -> String
part1 n = concatMap show . take 10 . drop n $ scores

part2 :: Int -> Int
part2 n = go 0 scores
  where
    digits  = map (read . (: [])) (show n)
    len     = length digits
    go i xs = if take len xs == digits
                then i
                else go (i + 1) (tail xs)

solveDay :: IO ()
solveDay = do
  steps <- read <$> readFile "input/14_day.txt"
  putStrLn $ "Part 1: " ++ part1 steps
  putStrLn $ "Part 1: " ++ show (part2 steps)