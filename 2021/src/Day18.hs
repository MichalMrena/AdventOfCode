module Day18 where

import           Data.Char ( ord )
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Data.Functor.Identity

data Tree = Leaf Int | Node Tree Tree deriving Show

reduce :: Tree -> Tree
reduce = id -- TODO

explode :: Tree -> Tree
explode = id -- TODO

split :: Tree -> Tree
split = id -- TODO

add :: Tree -> Tree -> Tree
add = Node

magnitude :: Tree -> Int
magnitude (Leaf x)   = x
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r




part1 :: [Tree] -> Int
part1 = magnitude . foldl1 (\acc x -> reduce (add acc x))

part2 :: String -> Int
part2 str = 0

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/18_day.txt" :: IO [Tree]
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  -- putStrLn $ "Part 2: " ++ (show . part2 $ input)
  print input

parseInput :: String -> [Tree]
parseInput = map (unpack . P.parse tree "") . lines

unpack :: Either P.ParseError c -> c
unpack     = either (error . show) id

tree :: P.ParsecT String u Data.Functor.Identity.Identity Tree
tree       = regular <|> pair

regular :: P.ParsecT String u Data.Functor.Identity.Identity Tree
regular    = Leaf . (subtract (ord '0') . ord) <$> P.digit

pair :: P.ParsecT String u Data.Functor.Identity.Identity Tree
pair       = do P.char '['
                t1 <- tree
                P.char ','
                t2 <- tree
                P.char ']'
                return (Node t1 t2)