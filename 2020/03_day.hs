import Data.Bool
import qualified Data.Vector.Unboxed as V

data Grid = Grid { rows :: Int
                 , cols :: Int
                 , grid :: V.Vector Char }

type Step = (Int, Int)
type Position = (Int, Int)

parseInput :: String -> Grid
parseInput s = Grid (length ls) (length . head $ ls) (V.fromList . concat $ ls)
    where
        ls = lines s

charAt :: Grid -> Position -> Char
charAt g (row, col') = let col   = col' `mod` (cols g)
                           index = row * (cols g) + col
                       in (grid g) V.! index

walk :: Grid -> Position -> Int -> Step -> Int
walk g (row, col) treeCount (right, down)
    | row >= (rows g) = treeCount
    | otherwise       = walk g (row + down, col + right) treeCount' (right, down)
    where
        treeCount' = treeCount + bool 0 1 ('#' == charAt g (row, col))

solvePart1 :: Grid -> Int
solvePart1 g = walk g (0, 0) 0 (3, 1)

solvePart2 :: Grid -> Int
solvePart2 g = let steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
               in product $ map (walk g (0, 0) 0) steps

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/03_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)