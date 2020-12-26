import Data.Maybe
import Data.List
import Data.List.Split
import Data.Tuple
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import qualified Data.Map as TM
import qualified Data.Set as S

data Food = Food { getIngredients :: [String]
                 , getAllergens   :: [String] } deriving Show
data Edge = Edge { getId       :: Int
                 , getFrom     :: Int
                 , getTo       :: Int
                 , getFlow     :: Int
                 , getCapacity :: Int } deriving Show
data EdgeWrap = Forward  { getEdge :: Edge }
              | Backward { getEdge :: Edge } deriving Show

type Edges = IntMap Edge
type Stars = IntMap [Int]
data Graph = Graph { getStars :: Stars, getEdges :: Edges }

addFlow :: Int -> Edge -> Edge
addFlow f (Edge i from to flow c) = Edge i from to (flow + f) c

fordFulkerson :: Int -> Int -> Graph -> Graph
fordFulkerson s t (Graph stars edges) = if null path
                                            then Graph stars edges
                                            else fordFulkerson s t graph'
    where
        path           = dfs S.empty $ s
        bottlenectFlow = minimum . map getFreeFlow $ path
        augmentedPath  = map augment path
        edges'         = foldl (\es e -> M.insertWith const (getId e) e es) edges augmentedPath
        graph'         = Graph stars edges'

        retrieveEdge             = \i -> if i < 0 then Backward (edges ! (-i)) else Forward (edges ! i)
        augment (Forward e)      = addFlow bottlenectFlow e
        augment (Backward e)     = addFlow (-bottlenectFlow) e
        getFreeFlow (Forward e)  = (getCapacity e) - (getFlow e)
        getFreeFlow (Backward e) = getFlow e
        getTarget (Forward e)    = getTo e
        getTarget (Backward e)   = getFrom e

        dfs visited v | isJust toSink = [fromJust toSink]
                      | isJust toNext = (fst . fromJust $ toNext) : (snd . fromJust $ toNext)
                      | otherwise     = []
            where
                visited'     = S.insert v visited
                allEdges     = map retrieveEdge $ (stars) ! v
                hasFreeFlow  = (> 0) . getFreeFlow
                isNotVisited = (not . (`S.member` visited) . getTarget)
                usableEdges  = filter isNotVisited . filter hasFreeFlow $ allEdges
                toSink       = find ((== t) . getTarget) usableEdges
                nexts        = map (dfs visited' . getTarget) $ usableEdges
                toNext       = find (not . null . snd) . zip usableEdges $ nexts

parseInput :: String -> [Food]
parseInput = map parseFood . lines
    where
        parseFood s = Food (words is) (tail . words . filter (/= ',') . init $ as)
            where
                [is, as] = splitOn "(" s

solvePart1 :: [Food] -> Int
solvePart1 fs = result
    where
        allIs  = S.fromList . concatMap getIngredients $ fs
        asMap  = foldl (\ms (Food is as) -> foldl (\ms' a -> TM.insertWith S.intersection a (S.fromList is) ms') ms as) TM.empty fs
        badIs  = foldl S.union S.empty . map snd . TM.toList $ asMap
        safeIs = S.difference allIs badIs
        result = sum . map length . map (\(Food is _) -> filter (`S.member` safeIs) is) $ fs

solvePart2 :: [Food] -> String
solvePart2 fs = result
    where
        asMap  = foldl (\ms (Food is as) -> foldl (\ms' a -> TM.insertWith S.intersection a (S.fromList is) ms') ms as) TM.empty fs

        sourceId       = 0
        sinkId         = 1
        allergens      = map fst . TM.toList $ asMap
        ingredients    = nub . concatMap (S.toList . snd) . TM.toList $ asMap
        idToAllergen   = M.fromList . zip [2 .. ]   $ allergens
        idToIngredient = M.fromList . zip [100 .. ] $ ingredients
        ingredientToId = TM.fromList . map swap . M.toList $ idToIngredient
        allergenToId   = TM.fromList . map swap . M.toList $ idToAllergen

        sourceToAllergens      = map (\a -> (sourceId, allergenToId TM.! a)) allergens
        ingredientsToSink      = map (\i -> (ingredientToId TM.! i, sinkId)) ingredients
        allergensToIngredients = concatMap makeAToIEdge . map (\(a, sis) -> (a, S.toList sis)) . TM.toList $ asMap
        makeAToIEdge (a, is)   = map (\i -> (allergenToId TM.! a, ingredientToId TM.! i)) is

        listOfEdges = map (\(i, (f, t)) -> Edge i f t 0 1) . zip [1 .. ] $ sourceToAllergens ++ allergensToIngredients ++ ingredientsToSink
        edges       = M.fromList . map (\e -> (getId e, e)) $ listOfEdges
        stars       = M.fromList . map makeStar $ [0, 1] ++ (M.keys idToAllergen) ++ (M.keys idToIngredient)
        makeStar v  = (v, (map getId . filter ((== v) . getFrom) $ listOfEdges) ++ (map (negate . getId) . filter ((== v) . getTo) $ listOfEdges))

        (Graph _ edges') = fordFulkerson sourceId sinkId (Graph stars edges)
        assignment = map (\(Edge _ f t _ _) -> (idToAllergen ! f, idToIngredient ! t))
                   . filter ((1 == ) . getFlow)
                   . filter (not . (== sinkId) . getTo)
                   . filter (not . (== sourceId) . getFrom)
                   . map snd
                   . M.toList $ edges'
        result = intercalate "," . map snd . sortOn fst $ assignment

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/21_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ solvePart2 input