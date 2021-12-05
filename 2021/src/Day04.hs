{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Day04 where

import qualified Text.Parsec as P
import           Control.Applicative ( Alternative((<|>)) )
import           Control.Monad ( void )
import           Data.List ( transpose )

type Grid = [[(Int, Bool)]]

isWinning :: Grid -> Bool
isWinning grid = any and bools || any and bools'
  where bools  = map (map snd) grid
        bools' = transpose bools

markNum :: Int -> Grid ->Grid
markNum num = map (map (\(n, b) -> (n, b || n == num)))

part1 :: ([Int], [Grid]) -> Int
part1 (xs, initGrids) = n * (sum . map fst . filter (not . snd) . concat $ gs)
  where
      (n, gs) = go xs initGrids

      go :: [Int] -> [Grid] -> (Int, Grid)
      go (n : ns) gs = case dropWhile (not . isWinning) gs' of
                         []    -> go ns gs'
                         g : _ -> (n, g)
        where gs' = map (markNum n) gs

part2 :: ([Int], [Grid]) -> Int
part2 (xs, initGrids) = n * (sum . map fst . filter (not . snd) . concat $ gs)
  where
      (n, gs) = go xs initGrids

      go :: [Int] -> [Grid] -> (Int, Grid)
      go (n : _)  [g] = (n, markNum n g)
      go (n : ns) gs  = go ns gs'
        where gs' = filter (not . isWinning) . map (markNum n) $ gs

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/04_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where parseInput str = case P.parse bingo "" str of
                           (Right (rs, gs)) -> (rs, addMarks gs)
                           (Left e)         -> error (show e)
        addMarks   = map (map (map (, False)))
        bingo      = (,) <$> (randoms <* P.skipMany P.space) <*> grids
        grids      = P.many1 grid
        grid       = P.count 5 gridLine <* eolOrEof
        gridLine   = P.count 5 (spaces >> naturalNum) <* eolOrEof
        randoms    = naturalNum `P.sepBy` P.char ','
        naturalNum = read <$> P.many1 P.digit
        spaces     = P.skipMany (P.char ' ')
        eolOrEof   = void P.newline <|> P.eof