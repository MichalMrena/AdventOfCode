import Prelude hiding (Left, Right)

data Action = North Int | East Int | South Int | West Int | Left Int | Right Int | Forward Int deriving (Show)
type Position = (Int, Int)

parseInput :: String -> [Action]
parseInput = map parseAction . lines
    where
        parseAction (c : n) = (parseChar c) (read n)
        parseAction a       = error $ a ++ " => Invalid action."
        parseChar 'N'       = North
        parseChar 'E'       = East
        parseChar 'S'       = South
        parseChar 'W'       = West
        parseChar 'L'       = Left
        parseChar 'R'       = Right
        parseChar 'F'       = Forward
        parseChar c         = error (c : " => Invalid char.")

moveShip :: [Action] -> Position
moveShip = fst . foldl applyAction ((0, 0), 90)
    where
        applyAction ((x, y), a) (North n)   = ((x, y + n), a)
        applyAction ((x, y), a) (East n)    = ((x + n, y), a)
        applyAction ((x, y), a) (South n)   = ((x, y - n), a)
        applyAction ((x, y), a) (West n)    = ((x - n, y), a)
        applyAction ((x, y), a) (Left n)    = ((x, y), (a - n) `mod` 360)
        applyAction ((x, y), a) (Right n)   = ((x, y), (a + n) `mod` 360)
        applyAction ((x, y), a) (Forward n) = applyAction ((x, y), a) (angleToAction a n)
        angleToAction a n = [North, East, South, West] !! (a `div` 90) $ n

solvePart1 :: [Action] -> Int
solvePart1 as = let (x, y) = moveShip as
                in (abs x) + (abs y)

moveShipRelative :: [Action] -> Position
moveShipRelative as = (sx', sy')
    where
        ((sx', sy'), _)               = foldl applyAction ((0, 0), (10, 1)) as
        rotateClockwise (x, y)        = (y, -x)
        rotateCounterClockwise (x, y) = (-y, x)
        applyAction (s, (wx, wy)) (North n)          = (s, (wx, wy + n))
        applyAction (s, (wx, wy)) (East n)           = (s, (wx + n, wy))
        applyAction (s, (wx, wy)) (South n)          = (s, (wx, wy - n))
        applyAction (s, (wx, wy)) (West n)           = (s, (wx - n, wy))
        applyAction (s, w) (Left n)                  = (s, last . take (1 + n `div ` 90) . iterate rotateCounterClockwise $ w)
        applyAction (s, w) (Right n)                 = (s, last . take (1 + n `div ` 90) . iterate rotateClockwise $ w)
        applyAction ((sx, sy), (wx, wy)) (Forward n) = ((sx + n * wx, sy + n * wy), (wx, wy))

solvePart2 :: [Action] -> Int
solvePart2 as = let (x, y) = moveShipRelative as
                in (abs x) + (abs y)

main :: IO ()
main = do
    as <- parseInput <$> readFile "./input/12_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ as)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ as)