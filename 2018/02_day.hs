import Data.List
import Data.Maybe

solvePart1 :: [String] -> Int
solvePart1 ids = twos * threes
    where
        fs     = map (filter (`elem` [2, 3]) . map length . group . sort) ids
        twos   = length . filter (2 `elem`) $ fs
        threes = length . filter (3 `elem`) $ fs

solvePart2 :: [String] -> String
solvePart2 ids = cs
    where
        ps   = [(i1, i2) | i1 <- ids, i2 <- ids, i1 /= i2]
        dist = length . filter (uncurry (/=)) . uncurry zip
        res  = fromJust . find ((== 1) . dist) $ ps
        cs   = map fst . filter (uncurry (==)) . uncurry zip $ res

main :: IO ()
main = do
    input <- lines <$> readFile "./input/02_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)