import Data.List.Split

type PassRecord = (Char, Int, Int, String)

parseInput :: String -> [PassRecord]
parseInput = map parseRecord . lines
    where
        parseRecord str = (c, l, u, pass)
            where
                [range, c : _ : [], pass] = words str
                [l, u] = map read . splitOn "-" $ range

solvePart1 :: [PassRecord] -> Int
solvePart1 = length . filter isValid
    where
        isValid (c, l, u, cs) = inRange l u (c `elemCount` cs)
        elemCount x           = length . filter (== x)
        inRange l u x         = x >= l && x <= u

solvePart2 :: [PassRecord] -> Int
solvePart2 = length . filter isValid
    where
        isValid (c, p1, p2, cs) = (cs !! (p1 - 1) == c) /= (cs !! (p2 - 1) == c)

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/02_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)