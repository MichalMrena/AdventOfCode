import Data.Map (Map, (!))
import qualified Data.Map as M

type BagName = String
type Bag     = [(Int, BagName)]
type BagMap  = Map BagName Bag

parseInput :: String -> BagMap
parseInput = foldl parseBag M.empty . lines
    where
        parseBag bagMap str = insertBag bagMap $ words $ filter (/= ',') $ init str
            where
                insertBag bm (hue : color : _ : _ : rest) = M.insert (hue ++ color) (parseRest rest) bm
                insertBag _ _ = error "not good"
                parseRest [] = []
                parseRest ["no", "other", "bags"] = []
                parseRest (count : hue : color : _ : rest) = (read count, hue ++ color) : (parseRest rest)
                parseRest _ = error "not good"

solvePart1 :: BagMap -> Int
solvePart1 bm = length . filter (hasGoldBag bm) $ (M.keys bm)
    where
        hasGoldBag bagMap current = ("shinygold" `elem` subBags) || (any (hasGoldBag bagMap) subBags)
            where
                subBags = map snd $ bagMap ! current

solvePart2 :: BagMap -> Int
solvePart2 bm = tbc "shinygold"
    where
        tbc bn = let subBags      = bm ! bn
                     subBagCounts = map fst subBags
                     subBagNames  = map snd subBags
                 in if null subBags
                     then 1
                     else (+ 1) . sum . zipWith (*) subBagCounts . map tbc $ subBagNames

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/07_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)