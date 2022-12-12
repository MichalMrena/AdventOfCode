module Day09 where

import           Data.Bifunctor
import           Data.List ( foldl', nub, sort )
import qualified Data.Set as S
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

data Move  = R Int | L Int | U Int | D Int deriving Show
type Point = (Int, Int)

pOp :: (Int -> Int -> Int) -> Point -> Point -> Point
pOp f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

(<->) :: Point -> Point -> Point
(<->) = pOp (-)

(<+>) :: Point -> Point -> Point
(<+>) = pOp (+)

vectorize :: [Move] -> [Point]
vectorize = concatMap f
  where f (R d) = replicate d (1, 0)
        f (L d) = replicate d (-1, 0)
        f (U d) = replicate d (0, -1)
        f (D d) = replicate d (0, 1)

moveTail :: Point -> Point -> Point
moveTail ph pt = pt'
  where diff@(dx, dy) = ph <-> pt
        needMove      = (abs dx) >= 2 || (abs dy) >= 2
        adjustDiff s  = (s `rem` 2) + (s `quot` 2)
        diff'         = bimap adjustDiff adjustDiff diff
        pt'           = if needMove then pt <+> diff' else pt

solve :: Int -> [Move] -> Int
solve knots = S.size . fst
            . foldl' f (S.singleton (0,0), ((0,0), replicate (knots-1) (0,0)))
            . vectorize
  where f (acc, (ph, pts)) m = (acc', (ph', pts'))
          where ph'  = ph <+> m
                pts' = go ph' pts
                acc' = S.insert (last pts') acc
                go _ [] = []
                go h (t:ts) = t' : go t' ts
                  where t' = moveTail h t

part1 :: [Move] -> Int
part1 = solve 2

part2 :: [Move] -> Int
part2 = solve 10

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/09_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where parseInput = unpack . P.parse moves ""
        unpack     = either (error . show) id
        nat        = read  <$> P.many1 P.digit
        moves      = move `P.sepBy` P.newline
        move       = (R <$> (P.string "R " *> nat))
                 <|> (L <$> (P.string "L " *> nat))
                 <|> (U <$> (P.string "U " *> nat))
                 <|> (D <$> (P.string "D " *> nat))