import Control.Parallel.Strategies
import Data.List ( scanl', maximumBy, transpose )
import Data.Ord ( comparing )
import Data.Array ( (!) )
import qualified Data.Array.Unboxed as A

calcAreaSums :: Int -> A.Array (Int, Int) Int
calcAreaSums serial = A.listArray ((0, 0), (300, 300))
                    $ concat
                    $ transpose
                    $ scanl buildCol (replicate 301 0) [1 .. 300]
    where
        buildCol prevRow y = zipWith (+) prevRow $ scanl' (buildRow y) 0 [1 .. 300]
        buildRow y prev x = prev + cellVal x y
        cellVal x y = let rackId = x + 10
                      in ((rackId * y + serial) * rackId) `mod` 1000 `div` 100 - 5

maxSquare :: A.Array (Int, Int) Int -> Int -> ((Int, Int, Int), Int)
maxSquare ps s = maximumBy (comparing snd)
               . map (\p@(x, y) -> ((y, x, s), squareSum p))
               . A.range $ ((1, 1), (301 - s, 301 - s))
    where
        squareSum (x, y) = (ps ! (y + s - 1, x + s - 1))
                         - (ps ! (y - 1,     x + s - 1))
                         - (ps ! (y + s - 1, x - 1    ))
                         + (ps ! (y - 1,     x - 1    ))

solvePart1 :: A.Array (Int, Int) Int -> (Int, Int)
solvePart1 ps = (\(x, y, _) -> (x, y)) . fst . maxSquare ps $ 3

solvePart2 :: A.Array (Int, Int) Int -> (Int, Int, Int)
solvePart2 ps = fst . maximumBy (comparing snd) . parMap rpar (maxSquare ps) $ [3 .. 300]

main :: IO ()
main = do
    serial <- read <$> readFile "./input/11_day.txt"
    let grid = calcAreaSums serial
    putStrLn $ "Part 1: " ++ show (solvePart1 grid)
    putStrLn $ "Part 2: " ++ show (solvePart2 grid)