import Data.List ( nub, sort )

part1 :: [[String]] -> Int
part1 = length . filter isValid
  where
    isValid ps = length ps == (length . nub $ ps)

part2 :: [[String]] -> Int
part2 = length . filter isValid
  where
    isValid = do l1 <- length
                 l2 <- length . nub . map sort
                 return (l1 == l2)

main :: IO ()
main = do
    pss <- map words . lines <$> readFile "./input/04_day.txt"
    putStrLn $ "Part 1: " ++ (show . part1 $ pss)
    putStrLn $ "Part 2: " ++ (show . part2 $ pss)