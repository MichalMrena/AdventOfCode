type Plants   = [Char]
type Patterns = [[Char]]

grow :: Patterns -> (Int, Plants) -> (Int, Plants)
grow ts (o, ps) = (o', ps')
    where
        ps'' = go ("...." ++ ps ++ "....")
        ps'  = dropWhile (== '.') ps''
        o'   = o + (length (takeWhile (== '.') ps'') - 2)

        go ps@[_, _, _, _, _] = ['#' | ps `elem` ts]
        go ps                 = case ps' of
                                    [] -> if c == '.' then [] else c : ps'
                                    _  -> c : ps'
            where
                c   = if take 5 ps `elem` ts then '#' else '.'
                ps' = go (tail ps)

eval :: (Integer, Plants) -> Integer
eval (o, ps) = sum . map fst . filter (('#' ==) . snd) . zip [o, (o + 1) ..] $ ps

solvePart1 :: Patterns -> Plants -> Integer
solvePart1 ts ps = eval (fromIntegral o, ps')
    where
        (o, ps') = iterate (grow ts) (0, ps) !! 20

solvePart2 :: Patterns -> Plants -> Integer
solvePart2 ts ps = eval (fromIntegral o', ps')
    where
        stable   = 2000
        (o, ps') = iterate (grow ts) (0, ps) !! stable
        o'       = 50000000000 - (stable - o)

main :: IO ()
main = do
    (ts, ps) <- parseInput . lines <$> readFile "./input/12_day.txt"
    putStrLn $ "Part 1: " ++ show (solvePart1 ts ps)
    putStrLn $ "Part 2: " ++ show (solvePart2 ts ps)
    where
        parseInput ls = (ts, ps)
            where
                ts = map (take 5) . filter ((== '#') . last) . drop 2 $ ls
                ps = drop 15 . head $ ls