import Data.Char

reduce :: String -> String
reduce = foldl eat ""
    where
        doAnihilate = \c1 c2 -> (isUpper c1) /= (isUpper c2) && (toUpper c1) == (toUpper c2)
        eat [] c = [c]
        eat (a : as) c | doAnihilate c a = as
                       | otherwise       = (c : a : as)

solvePart1 :: String -> Int
solvePart1 = length . reduce

solvePart2 :: String -> Int
solvePart2 cs = minimum . map length . map reduce $ allcs
    where
        allcs   = filters <*> [cs]
        filters = filter <$> notEq <$> ['a' .. 'z']
        notEq   = \c' c -> toLower c /= c'

main :: IO ()
main = do
    input <- readFile "./input/05_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)