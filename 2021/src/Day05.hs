{-# LANGUAGE FlexibleContexts #-}
module Day05 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import           Data.List ( foldl' )

type Point   = (Int, Int)
type Line    = (Point, Point)
data UseDiag = YesDiag | NoDiag deriving (Show, Eq)

solve :: UseDiag -> [Line] -> Int
solve ud = length . filter (>=2) . map snd . M.toList . foldl' addPoints M.empty
    where addPoints ms l@((x1, y1), (x2, y2))
            | y1 == y2  = foldl' addPoint ms [(x, y1) | x <- range x1 x2]
            | x1 == x2  = foldl' addPoint ms [(x1, y) | y <- range y1 y2]
            | otherwise = if ud == YesDiag
                            then foldl' addPoint ms (diagRange l)
                            else ms
          addPoint ms p = M.insertWith (+) p 1 ms

          range a b | a < b      = [a .. b]
                    | otherwise  = [b .. a]

          diagRange ((x1, y1), (x2, y2))
            = [(x1 + s1 * a, y1 + s2 * a) | a <- [0 .. (abs (x1 - x2))] ]
            where s1 = signum (x2 - x1)
                  s2 = signum (y2 - y1)

part1 :: [Line] -> Int
part1 = solve NoDiag

part2 :: [Line] -> Int
part2 = solve YesDiag

solveDay :: IO ()
solveDay = do
    lines <- readInput <$> readFile "input/05_day.txt" :: IO [Line]
    putStrLn $ "Part 1: " ++ (show . part1 $ lines)
    putStrLn $ "Part 2: " ++ (show . part2 $ lines)
    where readInput str = case P.parse lines "" str of
                            (Right ls) -> ls
                            (Left e)   -> error (show e)
          lines         = line `P.endBy` P.newline
          line          = (,) <$> (point <* P.string " -> ") <*> point
          point         = (,) <$> (naturalNum <* comma) <*> naturalNum
          naturalNum    = read <$> P.many1 P.digit
          comma         = P.char ','