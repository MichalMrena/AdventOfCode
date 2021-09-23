import qualified Data.Set as S
import qualified Data.Map as M
import           Data.Vector.Unboxed ((!), (//))
import qualified Data.Vector.Unboxed as V

realloc :: V.Vector Int -> V.Vector Int
realloc xs = V.imap (\i x -> if i < ti then succ x else x)
           . V.imap (\i x -> if i > mi then succ x else x)
           . V.map (+inc)
           $ xs'
    where
        len   = V.length xs
        mi    = V.maxIndex xs
        mv    = xs ! mi
        xs'   = xs // [(mi, 0)]
        restv = mv - (len - mi - 1)
        inc   = restv `div` len
        ti    = restv `mod` len

part1 :: V.Vector Int -> Int
part1 banks = go (S.singleton banks) banks 1
    where
        xss = iterate realloc banks

        go prev xs n
            | xs' `S.member` prev = n
            | otherwise           = go prev' xs' (n + 1)
            where
                xs'   = realloc xs
                prev' = S.insert xs' prev

part2 :: V.Vector Int -> Int
part2 banks = go (M.singleton banks 0) banks 1
    where
        xss = iterate realloc banks

        go prev xs n = case M.lookup xs' prev of
                           (Just i) -> n - i
                           Nothing  -> go prev' xs' (n + 1)
            where
                xs'   = realloc xs
                prev' = M.insert xs' n prev

main :: IO ()
main = do
    xs <- V.fromList . map read . words <$> readFile "./input/06_day.txt"
    putStrLn $ "Part 1: " ++ (show . part1 $ xs)
    putStrLn $ "Part 2: " ++ (show . part2 $ xs)