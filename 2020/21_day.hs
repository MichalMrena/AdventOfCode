import Data.Maybe
import Data.List
import Data.List.Split
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M
import qualified Data.Set as S

type Ingredients = [String]
type Allergens = [String]
data Food = Food Ingredients Allergens deriving Show

data Edge = EdgeData { getId       :: Int
                     , getFrom     :: Int
                     , getTo       :: Int
                     , getFlow     :: Int
                     , getCapacity :: Int } deriving Show
data EdgeWrap = Forward  { getEdge :: Edge }
              | Backward { getEdge :: Edge } deriving Show

type Edges  = IntMap Edge -- TODO possibly Vector
type Star   = [Int]
type Stars  = IntMap Star -- TODO possibly Vector
data Graph  = Graph { getStars :: Stars, getEdges :: Edges }

fordFulkerson :: Int -> Int -> Graph -> Graph
fordFulkerson s t (Graph stars edges) = if null path
                                            then Graph stars edges
                                            else fordFulkerson s t graph'
    where
        path           = map retrieveEdge . dfs S.empty $ s
        bottlenectFlow = minimum . map getFreeFlow $ path
        augmentedPath  = path -- TODO add bottleneck for forward, subtract bottleneck from
        graph'         = Graph stars edges

        retrieveEdge             = \i -> if i < 0 then Backward (edges ! (-i)) else Forward (edges ! i)
        getFreeFlow (Forward e)  = (getCapacity e) - (getFlow e)
        getFreeFlow (Backward e) = (getCapacity e) - (getFlow e)
        toId (Forward e)         = getId e
        toId (Backward e)        = negate . getId $ e

        dfs visited v | isJust toSink = [toId . fromJust $ toSink]
                      | isJust toNext = (toId . fst . fromJust $ toNext) : (snd . fromJust $ toNext)
                      | otherwise     = []
            where
                visited'     = S.insert v visited
                allEdges     = map retrieveEdge $ (stars) ! v
                hasFreeFlow  = (> 0) . getFreeFlow
                isNotVisited = (not . (`S.member` visited) . getTo . getEdge)
                usableEdges  = filter isNotVisited . filter hasFreeFlow $ allEdges
                toSink       = find ((== t) . getTo . getEdge) usableEdges
                nexts        = map (dfs visited' . getTo . getEdge) $ usableEdges
                toNext       = find (not . null . snd) . zip usableEdges $ nexts

parseInput :: String -> [Food]
parseInput = map parseFood . lines
    where
        parseFood s = Food (words is) (tail . words . init $ as)
            where
                [is, as] = splitOn "(" s

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/21_day.txt"
    let allergenes = nub $ concat $ map (\(Food _ as) -> as) input
    let ingredients = nub $ concat $ map (\(Food is _) -> is) input
    print (length allergenes)
    print (length ingredients)