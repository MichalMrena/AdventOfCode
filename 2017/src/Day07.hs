{-# LANGUAGE FlexibleContexts #-}

module Day07 where

import qualified Text.Parsec as P
import           Control.Applicative ( Alternative((<|>)) )
import           Data.List ((\\), nub)
import qualified Data.Map as M

data Node = Node { weight_ :: Int
                 , sons_   :: [String] } deriving Show

part1 :: M.Map String Node -> String
part1 graph = head (allNodes \\ nub notRoots)
  where pairs    = M.toList graph
        allNodes = map fst pairs
        notRoots = concatMap (sons_ . snd) pairs

solveDay :: IO ()
solveDay = do
  graph <- readGraph <$> readFile "input/07_day.txt"
  putStrLn $ "Part 1: " ++ part1 graph

  where
    parseLine ms s = case P.parse node "" s of
                       Right (k, p) -> M.insert k p ms
                       Left e       -> error (show e)
    readGraph = foldl parseLine M.empty . lines

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