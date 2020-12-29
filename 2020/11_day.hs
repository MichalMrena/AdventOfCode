import Data.Vector (Vector, (!))
import Control.Applicative (liftA2)
import qualified Data.Vector as V

data Cell = Floor | Empty | Taken deriving (Show, Eq)
data Grid = Grid { rowCount :: Int
                 , colCount :: Int
                 , cells    :: Vector Cell }
type Adjecent = Grid -> Int -> [Cell]

parseInput :: String -> Grid
parseInput str = let ls = lines str
                     rc = length ls
                     cc = length . head $ ls
                     cs = V.fromList . map charToCell . concat $ ls
                 in Grid rc cc cs
    where
        charToCell '.' = Floor
        charToCell 'L' = Empty
        charToCell '#' = Taken
        charToCell c   = error $ "Unknown char: " ++ [c]

directions :: [(Int, Int)]
directions = filter (/= (0, 0)) $ liftA2 (,) [-1, 0, 1] [-1, 0, 1]

inGrid :: Grid -> (Int, Int) -> Bool
inGrid (Grid rc cc _) (r, c) = r >= 0 && r < rc && c >=0 && c < cc

addPairs :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPairs (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

at :: Grid -> (Int, Int) -> Cell
at (Grid _ cc cs) (r, c) = cs ! (r * cc + c)

neighbours :: Adjecent
neighbours g@(Grid rc cc _) i = let pos = (i `div` cc, i `mod` rc)
                                in map (at g) . filter (inGrid g) . map (addPairs pos) $ directions

visible :: Adjecent
visible g@(Grid rc cc _) i = map head . filter (not . null) . map searchDirection $ directions
    where
        pos               = (i `div` cc, i `mod` rc)
        searchDirection d = dropWhile (== Floor)
                          . map (at g)
                          . takeWhile (inGrid g)
                          . tail
                          . iterate (addPairs d) $ pos

simulate :: Grid -> Adjecent -> Int -> Grid
simulate g@(Grid rc cc cs) adjecent threshold
    | cs == cs' = Grid rc cc cs
    | otherwise = simulate (Grid rc cc cs') adjecent threshold
    where
        cs'              = V.imap evalCell cs
        evalCell _ Floor = Floor
        evalCell i Empty = if (        0 == Taken `elemCount` (adjecent g i)) then Taken else Empty
        evalCell i Taken = if (threshold <= Taken `elemCount` (adjecent g i)) then Empty else Taken
        elemCount x xs   = length . filter (== x) $ xs

solvePart1 :: Grid -> Int
solvePart1 g = let g' = simulate g neighbours 4
               in V.length . V.filter (== Taken) $ (cells g')

solvePart2 :: Grid -> Int
solvePart2 g = let g' = simulate g visible 5
               in V.length . V.filter (== Taken) $ (cells g')

main :: IO ()
main = do
    g <- parseInput <$> readFile "./input/11_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ g)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ g)