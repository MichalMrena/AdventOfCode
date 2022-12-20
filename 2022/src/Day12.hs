module Day12 where

import           Data.Maybe ( fromMaybe )
import           Data.Char ( ord )
import           Data.List ( elemIndex, foldl' )
import qualified Data.Map.Strict as Mp
import           Data.Map.Strict ( (!) )
import qualified Data.Matrix as Mt
import           Data.Maybe ( isNothing, fromMaybe )
import qualified Data.Sequence as S
import           Data.Sequence ( Seq (Empty, (:<|)), (><) )

type Position = (Int, Int)
type Heights  = Mt.Matrix Char
type Canyon   = (Position, Position, Mt.Matrix Char)
type TestEdge = (Char -> Char -> Bool)

pointtoall :: TestEdge -> Position -> Heights -> [((Int, Int), Int)]
pointtoall test src heights = Mp.toList finaldists
  where nrow       = Mt.nrows heights
        ncol       = Mt.ncols heights
        finaldists = go (Mp.singleton src 0) (S.singleton src)

        go dists Empty      = dists
        go dists (p :<| ps) = go dists' ps'
          where
            (r, c) = p
            d      = dists ! p
            ns     = filter cango [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
            dists' = foldl' (\acc n -> Mp.insert n (d+1) acc) dists ns
            ps'    = ps >< (S.fromList ns)

            cango nei@(r', c') =
              (r' >= 1 && r' <= nrow) &&
              (c' >= 1 && c' <= ncol) &&
              (test (heights Mt.! nei) (heights Mt.! p)) &&
              (not (nei `Mp.member` dists))

part1 :: Canyon -> Int
part1 (src, dst, heights) = result
  where
        result       = fromMaybe undefined $ lookup dst finaldists
        finaldists   = pointtoall test src heights
        test to from = to <= succ from

part2 :: Canyon -> Int
part2 (_, dst, heights) = result
  where
    result       = minimum . map snd $ as
    as           = filter ((=='a') . (heights Mt.!) . fst) $ finaldists
    finaldists   = pointtoall test dst heights
    test to from = (cdiff to from >= -1)
    cdiff a b    = (ord a) - (ord b)

solveDay :: IO ()
solveDay = do
  xs@(s, d, m) <- parseInput <$> readFile "input/12_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where
    parseInput s = (src, dst, mat)
      where xss = lines s
            mat = Mt.setElem 'z' dst . Mt.setElem 'a' src . Mt.fromLists $ xss
            src = findPos xss 'S'
            dst = findPos xss 'E'
    findPos xss c = (row, 1 + fromMaybe undefined col)
      where mbs        = map (elemIndex c) xss
            (row, col) = head . dropWhile (isNothing . snd)
                       . zip [1..] $ mbs
