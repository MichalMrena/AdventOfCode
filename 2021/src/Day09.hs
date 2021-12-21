module Day09 where

import qualified Data.Set as S
import           Data.Set ( Set, member )
import           Data.List ( foldl', sortOn )
import           Data.Array ( Array, (!), bounds, listArray )
import           Data.Char ( ord )

lowpoints :: Array (Int, Int) Int -> [(Int, Int)]
lowpoints xss = [(r, c) | r <- [1 .. rc - 1], c <- [1 .. cc - 1], isMin r c ]
  where (_, (rc, cc)) = bounds xss
        isMin r c = x < (xss ! (r - 1, c))
                 && x < (xss ! (r + 1, c))
                 && x < (xss ! (r, c - 1))
                 && x < (xss ! (r, c + 1))
          where x = xss ! (r, c)

part1 :: Array (Int, Int) Int -> Int
part1 xss = sum . map ((+1) . (xss ! )) . lowpoints $ xss

part2 :: Array (Int, Int) Int -> Int
part2 xss = product . take 3 . sortOn negate . map S.size $ basins
  where
    basins = map (explore S.empty) (lowpoints xss)
    explore memo (r, c) | (r, c) `member` memo = memo
                        | otherwise = go (r, c + 1)
                                    $ go (r, c - 1)
                                    $ go (r + 1, c)
                                    $ go (r - 1, c) (S.insert (r, c) memo)
      where heigth = xss ! (r, c)
            go (r', c') m
              | heigth < target && target < 9 = explore m (r', c')
              | otherwise                     = m
                where target = xss ! (r', c')

solveDay :: IO ()
solveDay = do
  ls <- lines <$> readFile "input/09_day.txt"
  let rc  = 2 + length ls
      cc  = 2 + length (head ls)
      rxs = wrap cc (map (((10 : ) . (++ [10])) . parseLine) ls)
      xss = listArray ((0, 0), (rc - 1, cc - 1)) (concat rxs)
  putStrLn $ "Part 1: " ++ (show . part1 $ xss)
  putStrLn $ "Part 2: " ++ (show . part2 $ xss)
  where parseLine  = map (subtract (ord '0') . ord)
        wrap cc xs = [replicate cc 10] ++ xs ++ [replicate cc 10]