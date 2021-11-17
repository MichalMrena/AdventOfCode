module Day08 where

data Tree = Tree [Tree] [Int] deriving Show

parseTree :: [Int] -> Tree
parseTree ns = head . fst $ parseNodes 1 ([], ns)
  where
    parseNodes 0 (nodes, nums) = (nodes, nums)
    parseNodes n (nodes, nums) = parseNodes (n - 1) (node : nodes, drop dc nums')
      where
        (sc : dc : xs) = nums
        (sons, nums')  = parseNodes sc ([], xs)
        node           = Tree (reverse sons) (take dc nums')

solvePart1 :: [Int] -> Int
solvePart1 = dataSum . parseTree
  where
    dataSum (Tree sons ds) = sum ds + sum (map dataSum sons)

solvePart2 :: [Int] -> Int
solvePart2 = dataSum . parseTree
  where
    dataSum (Tree []   ds) = sum ds
    dataSum (Tree sons ds) = sum $ map dataSum sons'
      where
        sc    = length sons
        is    = map pred . filter (<= sc) . filter (> 0) $ ds
        sons' = map (sons !!) is

solveDay :: IO ()
solveDay = do
  input <- map read . words <$> readFile "input/08_day.txt"
  putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
  putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)