import Data.List

type Pass = String
type SeatPos = (Int, Int)

parseInput :: String -> [Pass]
parseInput = lines

ctoi :: Char -> Int
ctoi 'F' = 0
ctoi 'B' = 1
ctoi 'L' = 0
ctoi 'R' = 1
ctoi _   = error "Not good."

twoPows :: [Int]
twoPows = 1 : map (* 2) twoPows

passToSeatPos :: Pass -> SeatPos
passToSeatPos ps = let digits = map ctoi ps
                       (rowDigits, colDigits) = splitAt 7 digits
                       row = sum $ zipWith (*) (reverse rowDigits) twoPows
                       col = sum $ zipWith (*) (reverse colDigits) twoPows
                   in (row, col)

seatId :: Pass -> Int
seatId ps = let (row, col) = passToSeatPos ps
            in 8 * row + col

solvePart1 :: [Pass] -> Int
solvePart1 = maximum . map seatId

solvePart2 :: [Pass] -> Int
solvePart2 ps = let ids = sort . map seatId $ ps
                    pos = find (\(x, _, z) -> (z - x) > 2) $ zip3 ids (drop 1 ids) (drop 2 ids)
                in case pos of
                    Nothing        -> error "Not good"
                    Just (_, y, _) -> 1 + y

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/05_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)