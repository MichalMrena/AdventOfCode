module Day06 where

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V

realloc :: V.Vector Int -> V.Vector Int
realloc xs = V.imap (\i x -> if i < ti then succ x else x)
           . V.imap (\i x -> if i > mi then succ x else x)
           . V.map (+inc)
           $ xs'
  where
    len   = V.length xs
    mi    = V.maxIndex xs
    mv    = xs ! mi
    xs'   = xs // [(mi, 0)]
    restv = mv - (len - mi - 1)
    inc   = restv `div` len
    ti    = restv `mod` len

part1 :: V.Vector Int -> Int
part1 = go S.empty 0 . iterate realloc
  where
    go prevs n (xs : xss)
      | xs `S.member` prevs = n
      | otherwise           = go (S.insert xs prevs) (n + 1) xss

part2 :: V.Vector Int -> Int
part2 = go M.empty 0 . iterate realloc
  where
    go prevs n (xs : xss)
      = case M.lookup xs prevs of
          (Just i) -> n - i
          Nothing  -> go (M.insert xs n prevs) (n + 1) xss

solveDay :: IO ()
solveDay = do
  xs <- V.fromList . map read . words <$> readFile "input/06_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)