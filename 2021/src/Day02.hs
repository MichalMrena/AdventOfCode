module Day02 where

import Data.Char ( isSpace )

data Command = Down Int | Forward Int | Up Int deriving Show

part1 :: [Command] -> Int
part1 = uncurry (*) . foldl move (0, 0)
  where move (pos, depth) (Down a)    = (pos,     depth + a)
        move (pos, depth) (Forward a) = (pos + a, depth)
        move (pos, depth) (Up a)      = (pos,     depth - a)

part2 :: [Command] -> Int
part2 = (\(a, b, _) -> a * b) . foldl move (0, 0, 0)
  where move (pos, depth, aim) (Down a)    = (pos,     depth,           aim + a)
        move (pos, depth, aim) (Up a)      = (pos,     depth,           aim - a)
        move (pos, depth, aim) (Forward a) = (pos + a, depth + aim * a, aim)

solveDay :: IO ()
solveDay = do
  commands <- map parseLine . lines <$> readFile "input/02_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ commands)
  putStrLn $ "Part 2: " ++ (show . part2 $ commands)
  where parseLine ln = let command = takeWhile (not . isSpace) ln
                           arg     = read
                                   . drop 1
                                   . dropWhile (not . isSpace) $ ln
                       in readCommand command arg
        readCommand "down"    = Down
        readCommand "forward" = Forward
        readCommand "up"      = Up
