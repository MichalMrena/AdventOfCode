module Day04 where

import qualified Text.Parsec as P

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 = length . filter isIn
  where isIn (x, y) = check x y || check y x
          where check (l1, r1) (l2, r2) = (l1 >= l2 && r1 <= r2)

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 = length . filter overlaps
  where overlaps ((l1, r1), (l2, r2)) = not (r1 < l2 || r2 < l1)

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/04_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where parseInput = map (unpack . P.parse rangePair "") . lines
        unpack     = either (error . show) id
        rangePair  = (,) <$> (range <* comma) <*> range
        range      = (,) <$> (nat <* dash) <*> nat
        nat        = read <$> P.many1 P.digit
        dash       = P.char '-'
        comma      = P.char ','