module Day02 where

import qualified Text.Parsec as P

part1 :: [(Int, Int, Int)] -> Int
part1 = sum . map surface
  where surface (a, b, c) = let xs = [a*b, b*c, a*c]
                            in 2 * sum xs + minimum xs

part2 :: [(Int, Int, Int)] -> Int
part2 = sum . map surface
  where surface (a, b, c) = a*b*c + 2 * minimum [a+b, b+c, a+c]

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/02_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where parseInput = map (unpack . P.parse dims "") . lines
        unpack     = either (error . show) id
        nat        = read <$> P.many1 P.digit
        x          = P.char 'x'
        dims = (,,) <$> (nat <* x) <*> (nat <* x) <*> nat