import Data.IntMap (IntMap)
import qualified Data.IntMap as M

parseInput :: [Int] -> IntMap Int
parseInput =  M.fromList . zip [0 .. ]

playGame :: Int -> [Int] -> Int
playGame target xs = play initMemo (1 + length xs) (last xs)
    where
        initMemo = M.fromList . zip (init xs) $ [1, 2 .. ]
        play memo turn prev | turn == succ target = prev
                            | otherwise           = play memo' (succ turn) x
            where
                memo' = M.insertWith const prev (pred turn) memo
                x     = case prev `M.lookup` memo of
                            Nothing  -> 0
                            (Just i) -> turn - i - 1

solvePart1 :: [Int] -> Int
solvePart1 xs = playGame 2020 xs

solvePart2 :: [Int] -> Int
solvePart2 xs = playGame 30000000 xs

solve :: IO ()
solve = do
    xs <- read <$> readFile "./input/15_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ xs)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ xs)