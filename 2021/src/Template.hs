module DayXX where

import qualified Text.Parsec as P

part1 :: String -> Int
part1 str = 0

part2 :: String -> Int
part2 str = 0

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/XX_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  print input
  where parseInput = id
        -- parseInput = map (unpack . P.parse line "") . lines
        unpack     = either (error . show) id