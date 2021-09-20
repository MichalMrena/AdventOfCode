import Control.Monad.ST ( ST, runST )
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Array.Base as VM

jump :: (Int -> Int) -> VM.MVector s Int -> Int -> Int -> ST s Int
jump off xs n i = do
    step <- VM.read xs i
    let step' = off step
    VM.write xs i step'
    let i' = i + step
    if i' < 0 || i' >= VM.length xs
        then return n
        else jump off xs (n + 1) i'

part1 :: [Int] -> Int
part1 js = runST $ do
    xs <- V.unsafeThaw . V.fromList $ js
    jump succ xs 1 0

part2 :: [Int] -> Int
part2 js = runST $ do
    xs <- V.unsafeThaw . V.fromList $ js
    jump (\o -> if o >= 3 then pred o else succ o) xs 1 0

main :: IO ()
main = do
    xs <- map read . lines <$> readFile "./input/05_day.txt"
    putStrLn $ "Part 1: " ++ (show . part1 $ xs)
    putStrLn $ "Part 2: " ++ (show . part2 $ xs)