import Data.List.Split (splitOn)
import Data.Set (member)
import qualified Data.Set as S

parseInput :: String -> ([Int], [Int])
parseInput s = (parse p1s, parse p2s)
    where
        [p1s, p2s] = splitOn "\n\n" s
        parse = map read . tail . lines

solvePart1 :: ([Int], [Int]) -> Int
solvePart1 (deck1, deck2) = result
    where
        winnersDeck = play deck1 deck2
        result = sum $ zipWith (*) [1 .. ] (reverse winnersDeck)

        play ps1 [] = ps1
        play [] ps2 = ps2
        play (p1 : ps1) (p2 : ps2)
            | p1 > p2   = play (ps1 ++ [p1, p2]) ps2
            | otherwise = play ps1 (ps2 ++ [p2, p1])

data Player = First | Second deriving Show

solvePart2 :: ([Int], [Int]) -> Int
solvePart2 (deck1, deck2) = result
    where
        (_, winnersDeck) = play S.empty deck1 deck2
        result = sum $ zipWith (*) [1 .. ] (reverse winnersDeck)

        play _ ps1 [] = (First, ps1)
        play _ [] ps2 = (Second, ps2)
        play memo ps1@(p1 : ps1') ps2@(p2 : ps2')
            | (ps1 `member` memo) || (ps2 `member` memo) = (First, ps1)
            | (length ps1' >= p1) && (length ps2' >= p2) = case play S.empty (take p1 ps1') (take p2 ps2') of
                                                               (First, _)  -> play (S.fromList [ps1, ps2] `S.union` memo) (ps1' ++ [p1, p2]) ps2'
                                                               (Second, _) -> play (S.fromList [ps1, ps2] `S.union` memo) ps1' (ps2' ++ [p2, p1])
            | p1 > p2                                    = play (S.fromList [ps1, ps2] `S.union` memo) (ps1' ++ [p1, p2]) ps2'
            | otherwise                                  = play (S.fromList [ps1, ps2] `S.union` memo) ps1' (ps2' ++ [p2, p1])

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/22_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)