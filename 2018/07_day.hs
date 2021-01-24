import Data.Char
import Data.List
import Data.IntMap ((!))
import qualified Data.IntMap as M

ctoi :: Char -> Int
ctoi = (subtract (ord 'A')) . ord

itoc :: Int -> Char
itoc = chr . (+ (ord 'A'))

parseInput :: String -> [(Int, Int)]
parseInput = map (parseLine . words) . lines
    where
        parseLine cs = (ctoi (head $ cs !! 1), ctoi (head $ cs !! 7))

solvePart1 :: [(Int, Int)] -> String
solvePart1 arcs = reverse $ map itoc $ go inDegs []
    where
        vertices = nub $ map fst arcs ++ map snd arcs
        inDegs   = foldl (\m (_, t) -> M.adjust succ t m) (M.fromList $ zip vertices (repeat 0))  arcs :: M.IntMap Int
        fwStars  = foldl (\m (s, t) -> M.adjust (t:) s m) (M.fromList $ zip vertices (repeat [])) arcs

        pick inDs = case dropWhile ((> 0) . snd) $ M.toAscList inDs of
                      []           -> (inDs, -1)
                      ((s, _) : _) -> (lowerFs s, s)
            where
                lowerFs s = M.delete s $ foldl (\m t -> M.adjust pred t m) inDs (fwStars ! s)

        go inDs xs | x < 0     = xs
                   | otherwise = go inDs' (x : xs)
            where
                (inDs', x) = pick inDs

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/07_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)