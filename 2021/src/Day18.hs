module Day18 where

import           Data.List ( foldl1' )
import           Data.Char ( ord )
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

data Tree = Leaf Int | Node Tree Tree deriving Eq
data TransformedTree = Id | New Tree

reduce :: Tree -> Tree
reduce t = case (explode t, split t) of
             (New t', _) -> reduce t'
             (_, New t') -> reduce t'
             (_, _)      -> t

explode :: Tree -> TransformedTree
explode root = case go 0 root of
                 (_, _, False) -> Id
                 (_, root', _) -> New root'
  where
        go _ n@(Leaf _) = ((0, 0), n, False)

        go depth n@(Node (Leaf x) (Leaf y))
          | depth < 4 = ((0, 0), n, False)
          | otherwise = ((x, y), Leaf 0, True)

        go depth (Node l r)
          | doIncRight   = ((lx, 0), Node l' (incLeftmost ly r), True)
          | leftChanged  = ((lx, ly), Node l' r, True)
          | doIncLeft    = ((0, ry), Node (incRightmost rx l) r', True)
          | rightChanged = ((rx, ry), Node l r', True)
          | otherwise    = ((0, 0), Node l r, False)
          where
            ((lx, ly), l', leftChanged)  = go (depth + 1) l
            ((rx, ry), r', rightChanged) = go (depth + 1) r
            doIncLeft  = rightChanged && rx /= 0
            doIncRight = leftChanged && ly /= 0

        incLeftmost x' (Leaf x)   = Leaf (x + x')
        incLeftmost x' (Node l r) = Node (incLeftmost x' l) r

        incRightmost x' (Leaf x) = Leaf (x + x')
        incRightmost x' (Node l r) = Node l (incRightmost x' r)

split :: Tree -> TransformedTree
split = go
  where
    go (Leaf x)
      | x > 9     = New (Node (Leaf (div x 2)) (Leaf (div x 2 + mod x 2)))
      | otherwise = Id

    go (Node l r) = case (go l, go r) of
                      (New l', _) -> New (Node l' r)
                      (_, New r') -> New (Node l r')
                      (_, _)      -> Id

add :: Tree -> Tree -> Tree
add = Node

magnitude :: Tree -> Int
magnitude (Leaf x)   = x
magnitude (Node l r) = 3 * magnitude l + 2 * magnitude r

part1 :: [Tree] -> Int
part1 = magnitude . foldl1' (\acc x -> reduce (add acc x))

part2 :: [Tree] -> Int
part2 ts = maximum [magnitude (reduce (add x y)) | x <- ts
                                                 , y <- filter (/= x) ts ]
solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/18_day.txt" :: IO [Tree]
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where
    parseInput = map (unpack . P.parse tree "") . lines
    unpack     = either (error . show) id
    tree       = regular <|> pair
    regular    = Leaf . (subtract (ord '0') . ord) <$> P.digit
    pair       = do P.char '['
                    t1 <- tree
                    P.char ','
                    t2 <- tree
                    P.char ']'
                    return (Node t1 t2)