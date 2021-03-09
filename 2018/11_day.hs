import Data.List ( foldl', maximumBy )
import Data.Function ( on )
import Data.Vector.Unboxed ( (!), Vector )
import qualified Data.Vector.Unboxed as U

data Grid = Grid { rows  :: Int
                 , cols  :: Int
                 , cells :: Vector Int} deriving Show

generateGrid :: Int -> Grid
generateGrid serial = Grid 300 300 (U.fromList $ map cellVal coords)
    where
        rows    = 300
        cols    = 300
        cellVal = \(x, y) -> let rackId = x + 10
                             in ((rackId * y + serial) * rackId) `mod` 1000 `div` 100 - 5
        coords  = [(x, y) | y <- [1 .. rows], x <- [1 .. cols]]

at :: Grid -> (Int, Int) -> Int
at (Grid _ cc grid) (x, y) = grid ! ((y - 1) * cc + (x - 1))

pointPlus :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointPlus (lx, ly) (rx, ry) = (lx + rx, ly + ry)

solvePart1 :: Grid -> ((Int, Int), Int )
solvePart1 g@(Grid rc cc grid) = maximumBy (compare `on` snd) $ map (\p -> (p, squareSum p)) coords
    where
        coords       = [(x, y) | x <- [1 .. cc - 2], y <- [1 .. rc - 2]]
        squareSum    = sum . map (at g) . squareCoords
        squareCoords = \p -> pointPlus p <$> [(dx, dy) | dx <- [0 .. 2], dy <- [0 .. 2]]

main :: IO ()
main = do
    serial <- read <$> readFile "./input/11_day.txt"
    let grid = generateGrid serial

    putStrLn $ "Part 1: " ++ show (solvePart1 grid)