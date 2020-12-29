import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

parseInput :: String -> [[Set Char]]
parseInput = map (map S.fromList)
           . map lines
           . splitOn "\n\n"

solvePart1 :: [[Set Char]] -> Int
solvePart1 = sum
           . map S.size
           . map (foldl1 S.union)

solvePart2 :: [[Set Char]] -> Int
solvePart2 = sum
           . map S.size
           . map (foldl1 S.intersection)

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/06_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)