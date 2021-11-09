module Day07 where

import qualified Text.Parsec as P
import           Control.Applicative ( Alternative((<|>)) )
import           Data.List ((\\), nub)
import           Data.Map ((!))
import qualified Data.Map as M

data Node = Node { weight_ :: Int
                 , sons_   :: [String] } deriving Show

part1 :: M.Map String Node -> String
part1 graph = head (allNodes \\ nub notRoots)
  where allNodes = map fst (M.toList graph)
        notRoots = concatMap (sons_ . snd) (M.toList graph)

part2 :: M.Map String Node -> Int
part2 graph = case go (graph ! part1 graph) of
                (Right w) -> w
                _         -> undefined
  where
    go (Node w []) = Left w
    go (Node w sons) = case foldr (cncat . go . (graph !)) (Left []) sons of
                         (Right w) -> Right w
                         (Left ws) -> check ws
      where
        cncat _         (Right w) = Right w
        cncat (Right w) _         = Right w
        cncat (Left w)  (Left ws) = Left (w : ws)

        check ws = case filter neq $ zip sonWs (tail sonWs) of
                     [] -> Left (w + sum ws)
                     [((n1, w1), (n2, w2))]
                        -> if w1 == correctW
                             then Right (weight_ (graph ! n2) - abs (w1 - w2))
                             else Right (weight_ (graph ! n1) - abs (w1 - w2))
                     _  -> undefined
          where
            sonWs    = zip sons ws
            correctW = case filter eq $ zip sonWs (tail sonWs) of
                         []      -> undefined
                         (h : _) -> snd . fst $ h
            neq ((_, w1), (_, w2)) = w1 /= w2
            eq ((_, w1), (_, w2))  = w1 == w2

solveDay :: IO ()
solveDay = do
  graph <- readGraph <$> readFile "input/07_day.txt"
  putStrLn $ "Part 1: " ++ part1 graph
  putStrLn $ "Part 2: " ++ show (part2 graph)

  where
    readGraph = foldl parseLine M.empty . lines

    parseLine graph s = case P.parse node "" s of
                          Right (name, node) -> M.insert name node graph
                          Left e             -> error (show e)

    node :: P.Parsec String () (String, Node)
    node = do name <- P.many1 P.lower
              P.string " ("
              weight <- read <$> P.many1 P.digit
              P.string ")"
              ss <- sons <|> leaf
              return (name, Node weight ss)
      where
        sons = P.string " -> " >> P.many1 P.lower `P.sepBy1` P.string ", "
        leaf = P.eof >> return []