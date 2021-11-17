module Day04 where

import Data.List
import Data.List.Split
import Data.Function
import Data.Maybe
import qualified Data.IntMap as M

type GuardId = Int
type Minute  = Int
type Event   = Int

parseInput :: String -> [Event]
parseInput = map (parseLine . words) . sort . lines
  where
    parseLine (_ : ms : ws : rss)
      | "Guard" == ws = read . tail   . head   $ rss
      | otherwise     = read . take 2 . drop 3 $ ms
    parseLine _ = error "Invalid input."

sleepMinutes :: [Event] -> [(GuardId, [Minute])]
sleepMinutes = filter (not . null . snd) . M.toList . M.fromListWith (++) . shifts
  where
      intervalToList [a, b] = [a .. (b - 1)]
      intervalToList _      = error "Not an interval."

      shifts []       = []
      shifts (x : xs) = (x, ms) : shifts xs'
        where
          bs  = takeWhile (< 60) xs
          xs' = drop (length bs) xs
          ms  = foldl (++) [] . map intervalToList . chunksOf 2 $ bs

frequency :: [Int] -> [(Int, Int)]
frequency = M.toList . foldl (\m k -> M.insertWith (+) k 1 m) M.empty

maxFrequency :: [(Int, Int)] -> (Int, Int)
maxFrequency = maximumBy (compare `on` snd)

solvePart1 :: [Event] -> Int
solvePart1 es = laziestGuardId * laziestMinute
  where
    guardSleepMs    = sleepMinutes es
    totalSleepTimes = map (\(i, ms) -> (i, length ms)) guardSleepMs
    laziestGuardId  = fst . maximumBy (compare `on` snd) $ totalSleepTimes
    laziestMinute   = fst . maxFrequency . frequency . fromJust . lookup laziestGuardId $ guardSleepMs

solvePart2 :: [Event] -> Int
solvePart2 es = guardId * m
  where
    guardSleepMs      = sleepMinutes es
    guardLaziestMins  = map (\(i, ms) -> (i, maxFrequency . frequency $ ms)) $ guardSleepMs
    (guardId, (m, _)) = maximumBy (compare `on` (snd . snd)) $ guardLaziestMins

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/04_day.txt"
  putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
  putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)