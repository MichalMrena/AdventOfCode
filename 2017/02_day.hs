import Data.List ( foldl' )

part1 :: [[Int]] -> Int
part1 = sum . map check
  where check = (\(l, h) -> h - l) . minmax
        minmax xs = foldl' go (maxBound, minBound) xs
          where go (l, h) x = (min l x, max h x)

part2 :: [[Int]] -> Int
part2 = sum . map check
  where check xs = head [x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]

main :: IO ()
main = do
  xss <- parseInput <$> readFile "./input/02_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xss)
  putStrLn $ "Part 2: " ++ (show . part2 $ xss)
  where parseInput = map (map read . words) . lines