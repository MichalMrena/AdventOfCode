module Day08 where

import           Data.Char ( ord )
import           Data.List ( foldl', nub )
import           Data.Matrix ( Matrix, (!) )
import qualified Data.Matrix as M

part1 :: Matrix Int -> Int
part1 m = length $ nub $ concatMap (concatMap go) [ltor, rtol, ttod, dtot]
  where nr   = M.nrows m
        nc   = M.ncols m
        ltor = [ [(r, c) | c <- [1,2 .. nc]]     | r <- [1,2 .. nr] ]
        rtol = [ [(r, c) | c <- [nc, nc-1 .. 1]] | r <- [1,2 .. nr] ]
        ttod = [ [(r, c) | r <- [1,2 .. nr]]     | c <- [1,2 .. nc] ]
        dtot = [ [(r, c) | r <- [nr, nr-1 .. 1]] | c <- [1,2 .. nc] ]

        go []     = []
        go (p:ps) = p : go (dropWhile (\p' -> (m ! p') <= (m ! p)) ps)

part2 :: Matrix Int -> Int
part2 m = maximum (map score [(r, c) | r <- [1..nr], c <- [1..nc]])
  where nr = M.nrows m
        nc = M.ncols m
        view (r', c') = [ [(r', c) | c <- [c' + 1 .. nc] ]
                        , [(r', c) | c <- [c' - 1, c' - 2 .. 1]]
                        , [(r, c') | r <- [r' + 1 .. nr]]
                        , [(r, c') | r <- [r' - 1, r' - 2 .. 1]] ]

        score p = product $ map (length . go (m ! p)) $ view p

        go _ [] = []
        go h (p:ps) | (m ! p) < h  = p : go h ps
                    | (m ! p) >= h = [p]

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/08_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)

  where parseInput :: String -> Matrix Int
        parseInput = M.fromLists . map (map ctoi) . lines
        ctoi       = subtract (ord '0') . ord