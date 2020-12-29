import Data.List
import Data.Maybe

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod x n m | even n    = (r * r)     `mod` m
             | otherwise = (x * r * r) `mod` m
  where
    r = powMod x (n `div` 2) m

parseInput :: String -> (Integer, Integer)
parseInput = (\[pk1, pk2] -> (pk1, pk2)) . map read . lines

solvePart1 :: (Integer, Integer) -> Integer
solvePart1 (pk1, pk2) = powMod pk1 l2 20201227
    where
        l2 = fromIntegral . fromJust . elemIndex pk2 . iterate ((`mod` 20201227) . (* 7)) $ 1

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/25_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)