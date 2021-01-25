import Data.Char
import Data.List
import Data.IntMap ((!))
import qualified Data.IntMap as M

parseInput :: String -> [(Int, Int)]
parseInput = map (parseLine . words) . lines
    where
        ctoi         = (subtract (ord 'A')) . ord
        parseLine cs = (ctoi (head $ cs !! 1), ctoi (head $ cs !! 7))

solvePart1 :: [(Int, Int)] -> String
solvePart1 arcs = reverse $ map itoc $ go inDegs []
    where
        itoc     = chr . (+ (ord 'A'))
        vertices = nub $ map fst arcs ++ map snd arcs
        inDegs   = foldl (\m (_, t) -> M.adjust succ t m) (M.fromList $ zip vertices (repeat 0))  arcs :: M.IntMap Int
        fwStars  = foldl (\m (s, t) -> M.adjust (t:) s m) (M.fromList $ zip vertices (repeat [])) arcs

        go inDs xs | M.null inDs = xs
                   | otherwise   = go inDs' (x : xs)
            where
                x     = fst . head . dropWhile ((> 0) . snd) . M.toAscList $ inDs
                inDs' =  M.delete x $ foldl (\m t -> M.adjust pred t m) inDs (fwStars ! x)

solvePart2 :: [(Int, Int)] -> Int
solvePart2 arcs = go initInDegs initTasks (5 - length initTasks)
    where
        vertices   = nub $ map fst arcs ++ map snd arcs
        inDegs     = foldl (\m (_, t) -> M.adjust succ t m) (M.fromList $ zip vertices (repeat 0))  arcs :: M.IntMap Int
        fwStars    = foldl (\m (s, t) -> M.adjust (t:) s m) (M.fromList $ zip vertices (repeat [])) arcs
        initSrcs   = map fst . take 5 . takeWhile ((== 0) . snd) . sortOn snd . M.toAscList $ inDegs
        initInDegs = foldl (\m s -> M.delete s m) inDegs initSrcs
        initTasks  = map (\s -> (s + 61, s)) . sort $ initSrcs

        go _    []               _  = error "Shut up."
        go inDs ((time, s) : qs) wc
            | null queue' = time
            | otherwise   = go inDs'' queue' wc'
            where
                wc'    = succ wc
                inDs'  = foldl (\m t -> M.adjust pred t m) inDs (fwStars ! s)
                ready  = map fst . take wc' . takeWhile ((== 0) . snd) . sortOn snd . M.toAscList $ inDs'
                inDs'' = foldl (\m x -> M.delete x m) inDs' ready
                tasks  = map (\x -> (time + x + 61, x)) ready
                queue' = foldl (flip insert) qs tasks

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/07_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)