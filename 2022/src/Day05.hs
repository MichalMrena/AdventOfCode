module Day05 where

import           Data.Char ( isSpace )
import           Data.List ( transpose, foldl' )
import           Data.List.Split ( splitOn )
import           Data.Vector ( (!), (//), Vector )
import qualified Data.Vector as V
import qualified Text.Parsec as P

every :: Int -> [a] -> [a]
every n = go
  where go []     = []
        go (x:xs) = x : every n (drop (n-1) xs)

move :: ([Char] -> [Char]) -> Vector [Char] -> (Int, Int, Int) -> Vector [Char]
move f stacks (n, from, to) = stacks'
  where crates  = f (take n (stacks ! from))
        froms'  = drop n (stacks ! from)
        tos'    = crates ++ (stacks ! to)
        stacks' = stacks // [(from, froms'), (to, tos')]

moveCrates :: ([Char] -> [Char]) -> (Vector [Char], [(Int, Int, Int)]) -> [Char]
moveCrates f (stacks, moves) = map head (V.toList stacks')
  where stacks' = foldl' (move f) stacks moves'
        moves'  = map (\(n, from, to) -> (n, from-1, to-1)) moves

part1 :: (Vector [Char], [(Int, Int, Int)]) -> String
part1 = moveCrates reverse

part2 :: (Vector [Char], [(Int, Int, Int)]) -> String
part2 = moveCrates id

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/05_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where parseInput :: String -> (Vector [Char], [(Int, Int, Int)])
        parseInput s = (creates, moves)
          where [createLines, moveLines] = splitOn [""] (lines s)
                creates = V.fromList
                        . map (dropWhile isSpace . reverse)
                        . transpose
                        . map (every 4 . init . tail)
                        . reverse
                        . init $ createLines
                moves  = map (unpack . P.parse move "") moveLines
                unpack = either (error . show) id
                nat    = read <$> P.many1 P.digit
                move   = (,,) <$> (P.string "move " *> nat <* P.string " from ")
                              <*> (nat <* P.string " to ")
                              <*> nat