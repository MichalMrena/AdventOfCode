import Data.Vector.Unboxed (Vector, (!))
import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed as V

parseInput :: String -> Vector Int
parseInput = V.fromList . map read . lines

preambleSize :: Int
preambleSize = 25

findInvalid :: Vector Int -> IntSet -> Int -> Int
findInvalid xs ps i | canBeSum x = findInvalid xs ps' (succ i)
                    | otherwise  = x
    where
        x          = xs ! i
        old        = xs ! (i - preambleSize)
        ps'        = S.insert x . S.delete old $ ps
        canBeSum n = S.foldr (\p acc -> acc || (n - p) `S.member` ps) False ps

findSequence :: Vector Int -> Int -> Int -> Int -> Int -> (Int, Int)
findSequence xs target csum i j | csum == target = (i, j)
                                | otherwise     = findSequence xs target sum' i' j'
    where
        left  = xs ! i
        right = xs ! (j + 1)
        (i', j', sum') = if csum + right > target
                             then (succ i, j, csum - left)
                             else (i, succ j, csum + right)

solvePart1 :: Vector Int -> Int
solvePart1 xs = let preamble = S.fromList . V.toList . V.slice 0 preambleSize $ xs
                in findInvalid xs preamble preambleSize

solvePart2 :: Vector Int -> Int
solvePart2 xs = let target = solvePart1 xs
                    csum   = (xs ! 0) + (xs ! 1)
                    (i, j) = findSequence xs target csum 0 1
                    xs'    = V.slice i (j - i) xs
                in (V.minimum xs') + (V.maximum xs')

solve :: IO ()
solve = do
    xs <- parseInput <$> readFile "./input/09_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ xs)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ xs)