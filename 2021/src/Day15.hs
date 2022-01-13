module Day15 where

import           Data.Char ( ord )
import           Data.Function ( on )
import           Data.List ( minimumBy, foldl' )
import           Data.Array ( (!) )
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Bifunctor as BF

type Graph = A.Array (Int, Int) Int
type Node  = (Int, Int)
type Cost  = Int
type Queue = [(Node, Cost)]

solve :: Int -> Graph -> Cost
solve scale graph = go S.empty [((0, 0), 0)] - edgeCost (0, 0) + edgeCost dst
  where
        (baseRc, baseCc) = BF.bimap (+ 1) (+ 1) . snd . A.bounds $ graph
        rowCount         = scale * baseRc
        colCount         = scale * baseCc
        dst              = (rowCount - 1, colCount - 1)
        inGrid (r, c)    = r >= 0 && c >= 0 && r < rowCount && c < colCount
        shouldVisit visited node = not (node `S.member` visited) && inGrid node

        edgeCost :: Node -> Cost
        edgeCost (r, c) = if totalCost < 10
                            then totalCost
                            else 1 + (totalCost `mod` 10)
          where totalCost            = baseCost + tileRow + tileCol
                baseCost             = graph ! (rowOffset, colOffset)
                (tileRow, rowOffset) = r `divMod` baseRc
                (tileCol, colOffset) = c `divMod` baseCc

        go :: S.Set Node -> Queue -> Cost
        go visited queue | currNode == dst = totalCost
                         | otherwise       = go visited' queue''
          where (currNode, totalCost) = findMin queue
                (r, c)   = currNode
                newCost  = totalCost + edgeCost currNode
                nexts    = filter (shouldVisit visited)
                             [(r, c + 1), (r + 1, c), (r, c - 1), (r - 1, c)]
                visited' = S.insert currNode visited
                queue'   = delete currNode queue
                queue''  = foldl' (insertOrUpdate newCost) queue' nexts

        -- "priority queue" operations:

        delete :: Node -> Queue -> Queue
        delete x = filter ((/= x) . fst)

        findMin :: Queue -> (Node, Cost)
        findMin = minimumBy (compare `on` snd)

        insertOrUpdate :: Cost -> Queue -> Node -> Queue
        insertOrUpdate cost queue node = og queue
          where og []                       = [(node, cost)]
                og (x : xs) | fst x == node = if snd x > cost
                                                then (node, cost) : xs
                                                else x : xs
                            | otherwise     = x : og xs

part1 :: Graph -> Cost
part1 = solve 1

part2 :: Graph -> Cost
part2 = solve 5

-- TODO use proper prio queue and check time, takes 3,61s on hp laptop...
solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/15_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where parseInput str = A.listArray is . concatMap (map ctoi) $ ls
          where ls   = lines str
                ctoi = subtract (ord '0') . ord
                rows = length ls
                cols = length (head ls)
                is   = ((0, 0), (rows - 1, cols - 1))