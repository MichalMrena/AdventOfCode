import Data.List.Split

parseInput :: String -> (Int, [Int])
parseInput str = (read target, ids)
    where
        [target, rawids] = lines str
        ids              = map (read . replaceX) . splitOn "," $ rawids
        replaceX "x"     = "0"
        replaceX x       = x

findFirst :: Int -> [Int] -> (Int, Int)
findFirst target ids = head
                     . dropWhile ((== 0) . snd)
                     . map (\t -> (t, firstDivisor t))
                     . iterate succ $ target
    where
        firstDivisor t = fst
                       . head
                       . (++ [(0, 0)])
                       . dropWhile ((/= 0) . snd)
                       . zip ids
                       . map (t `mod`) $ ids

solvePart1 :: Int -> [Int] -> Int
solvePart1 target ids = let (t, i) = findFirst target . filter (/= 0) $ ids
                        in (t - target) * i

egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd a 0 = (a, a, 0)
egcd a b = (g, y, x - (a `div` b) * y)
  where
    (g, x, y) = egcd b (a `mod` b)

inverse :: Integer -> Integer -> Integer
inverse m a = y `mod` m
  where
    (_, _, y) = egcd m a

chineseRemainder :: [Integer] -> [Integer] -> Integer
chineseRemainder as ms = (`mod` bigm) . sum . zipWith3 (\a b b' -> a * b * b') as bs $ bs'
    where
        bigm = product ms
        bs   = map (bigm `div`) ms
        bs'  = zipWith (\b m -> inverse m b) bs ms

solvePart2 :: [Integer] -> Integer
solvePart2 ids = chineseRemainder (map fst rd) (map snd rd)
    where
        rs = zipWith subtract [0 ..] ids
        rd = filter ((/= 0) . snd) . zip rs $ ids

solve :: IO ()
solve = do
    (t, ids) <- parseInput <$> readFile "./input/13_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 t $ ids)
    putStrLn $ "Part 2: " ++ (show . solvePart2   $ map fromIntegral ids)