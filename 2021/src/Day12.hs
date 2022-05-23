module Day12 where

import           Data.Char
import           Data.List ( foldl' )
import qualified Data.Set as S
import qualified Text.Parsec as P

type Node  = String
type Edge  = (String, String)
type Graph = [Edge]
type Path  = [Node]

fwd :: Node -> Graph -> [Node]
fwd n = map (\(s, d) -> if s == n then d else s)
      . filter (\(s, d) -> s == n || d == n)

isSmall :: String -> Bool
isSmall = isLower . head

isBig :: String -> Bool
isBig = isUpper . head

part1 :: Graph -> Int
part1 graph = go S.empty "start"
  where go _    "end"   = 1
        go path node
          | node `S.member` path && isSmall node = 0
          | otherwise = sum . map (go path') $ fwd node graph
          where path' = S.insert node path

part2 :: Graph -> Int
part2 graph = S.size $ go ([], []) "start"
  where
    go (path, twice) node
      | node == "end"                                = S.singleton path'
      | revisited && node == "start"                 = S.empty
      | revisited && isSmall node && [node] /= twice = S.empty
      | isBig node                                   = union incTwice
      | revisited && isSmall node && [node] == twice = union notIncTwice
      | isSmall node                                 = union both
      where path'       = node : path
            revisited   = node `elem` path
            twice'      = node : twice
            incTwice    = map (go (path', twice))  (fwd node graph)
            notIncTwice = map (go (path', twice')) (fwd node graph)
            both        = incTwice ++ if null twice then notIncTwice else []
            union       = foldl' S.union S.empty

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/12_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where parseInput = map (unpack . P.parse edge "") . lines
        unpack     = either (error . show) id
        edge       = (,) <$> (nodeName <* P.char '-') <*> nodeName
        nodeName   = P.many1 P.letter