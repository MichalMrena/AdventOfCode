module Day14 where

import Data.List ( find )

import           Data.Set ( member )
import qualified Data.Set as S
import qualified Text.Parsec as P

type Point = (Int, Int)

part1 :: S.Set Point -> Int
part1 fixed = go fixed (500, 0)
  where lowest = maximum . map snd . S.toList $ fixed
        go fs p@(c, r)
          | r == lowest = S.size fs - S.size fixed
          | otherwise   = case find (\p' -> not $ p' `member` fs) dsts of
                            Nothing   -> go (S.insert p fs) (500, 0)
                            (Just p') -> go fs p'
          where dsts = [(c, r+1), (c-1, r+1), (c+1, r+1)]

part2 :: S.Set Point -> Int
part2 fixed = go fixed origin
  where lowest = (+2) . maximum . map snd . S.toList $ fixed
        origin = (500, 0)
        go fs p@(c, r)
          | r == (lowest-1) = go (S.insert p fs) origin
          | otherwise = case find (\p' -> not $ p' `member` fs) dsts of
                          (Just p') -> go fs p'
                          Nothing   -> if p == origin
                                         then 1 + S.size fs - S.size fixed
                                         else go (S.insert p fs) origin
          where dsts = [(c, r+1), (c-1, r+1), (c+1, r+1)]

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/14_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where
    parseInput = S.fromList . concatMap mkline . unpack . P.parse alllines ""
    unpack     = either (error . show) id
    comma      = P.char ','
    nat        = read <$> P.many1 P.digit
    point      = (,) <$> (nat <* comma) <*> nat
    arrow      = P.string " -> "
    line       = point `P.sepBy` arrow
    alllines   = line `P.sepBy` P.newline
    mkline ps  = concatMap (uncurry mkpts) $ zip ps (tail ps)
    mkpts (c1, r1) (c2, r2)
      | c1 == c2  = [(c1, r) | r <- [min r1 r2 .. max r1 r2]]
      | r1 == r2  = [(c, r1) | c <- [min c1 c2 .. max c1 c2]]
      | otherwise = undefined