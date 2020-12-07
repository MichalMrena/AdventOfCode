import Data.Map (Map, (!))
import qualified Data.Map as M

type BagName = String
type Bag = [(Int, BagName)]
type BagMap = Map BagName Bag

parseBag :: BagMap -> String -> BagMap
parseBag bagMap str = insertBag bagMap $ words $ filter (/= ',') $ init str
    where
        insertBag bagMap (hue : color : _ : _ : rest) = M.insert (hue ++ color) (parseRest rest) bagMap
        parseRest [] = []
        parseRest ["no", "other", "bags"] = []
        parseRest (count : hue : color : _ : rest) = (read count, hue ++ color) : (parseRest rest)

parseInput :: IO BagMap
parseInput = do
    bagLines <- lines <$> readFile "./input/07_day.txt"
    return $ foldl parseBag M.empty bagLines

canContainGoldBag :: BagMap -> BagName -> Bool
canContainGoldBag bagMap current = let subBags = map snd $ bagMap ! current
                                   in ("shinygold" `elem` subBags) || (any (canContainGoldBag bagMap) subBags)

totalBackCount :: BagMap -> BagName -> Int
totalBackCount bagMap bagName = let subBags      = bagMap ! bagName
                                    subBagCounts = map fst subBags
                                    subBagNames  = map snd subBags
                                in if (null subBags)
                                       then 1
                                       else (+ 1) . sum . zipWith (*) subBagCounts . map (totalBackCount bagMap) $ subBagNames

solve :: IO ()
solve = do
    bagMap <- parseInput
    let validBags = filter (canContainGoldBag bagMap) (M.keys bagMap)
    putStrLn $ "Part 1: " ++ (show . length $ validBags)
    putStrLn $ "Part 2: " ++ (show . totalBackCount bagMap $ "shinygold")