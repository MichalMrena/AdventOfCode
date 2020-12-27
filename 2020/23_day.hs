import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.List
import Data.STRef
import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

data Cup = Cup Int deriving Show
type ForwardList = Vector Int

parseInput :: String -> [Int]
parseInput = map (subtract (ord '0') . ord)

nexts :: Int -> Int -> ForwardList -> [Int]
nexts i m ms = tail $ scanl (\e _ -> ms ! e) m [0 .. i - 1]

moveCups :: Int -> [Int] -> ForwardList
moveCups n xs = runST $ do
    let xs'    = V.fromList $ map snd . sortOn fst $ (-1, -1) : (last xs, head xs) : zip xs (tail xs)
    let maxCup = maximum xs

    ms   <- V.thaw xs'
    hRef <- newSTRef (head xs)

    forM_ [0 .. n - 1] $ \_ -> do
        h  <- readSTRef hRef
        p1 <- VM.read ms h
        p2 <- VM.read ms p1
        p3 <- VM.read ms p2
        pt <- VM.read ms p3

        let i = head . dropWhile (`elem` [p1, p2, p3]) $ takeWhile (> 0) [h - 1, h - 2 .. ] ++ takeWhile (> h) [maxCup, maxCup - 1 ..]
        pn <- VM.read ms i

        VM.write ms h pt
        VM.write ms i p1
        VM.write ms p3 pn

        h' <- VM.read ms h
        writeSTRef hRef h'

    V.unsafeFreeze ms

solvePart1 :: [Int] -> String
solvePart1 = concatMap show . nexts 8 1 . moveCups2 100

solvePart2 :: [Int] -> Int
solvePart2 xs = product . nexts 2 1 . moveCups2 10000000 $ xs'
    where
        xs' = xs ++ takeWhile (<= 1000000) [10 .. ]

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/23_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)