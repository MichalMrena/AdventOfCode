module Day01 where

ctoi :: Char -> Int
ctoi '(' = 1
ctoi ')' = -1
ctoi _   = undefined

part1 :: String -> Int
part1 = sum . map ctoi

part2 :: String -> Int
part2 = go 1 0
  where go p (-1) _      = p - 1
        go p f    (c:cs) = go (p + 1) (f + ctoi c) cs
        go _ _    []     = undefined

solveDay :: IO ()
solveDay = do
  xs <- readFile "input/01_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)