module Day13 where

import qualified Text.Parsec as P
import qualified Data.Bifunctor as B
import           Data.List.Split ( splitOn )
import           Data.List ( partition, union, foldl' )
import           Control.Monad ( forM_ )

data FoldOn = X Int | Y Int deriving Show
type Point  = (Int, Int)

partOn :: FoldOn -> [Point] -> ([Point], [Point])
partOn (X x') = partition (\ (x, _) -> x < x')
partOn (Y y') = partition (\ (_, y) -> y < y')

foldOn :: FoldOn -> [Point] -> [Point]
foldOn (X x') = map (B.first ((2 * x') -))
foldOn (Y y') = map (B.second ((2 * y') -))

part1 :: ([(Int, Int)], [FoldOn]) -> Int
part1 (points, fold : _) = length (lhs `union` foldOn fold rhs)
  where (lhs, rhs) = partOn fold points

part2 :: ([(Int, Int)], [FoldOn]) -> IO ()
part2 (points, folds) = printPts
  where points'   = foldl' fold points folds
        fold ps f = lhs `union` foldOn f rhs
          where (lhs, rhs) = partOn f ps
        maxX      = maximum . map fst $ points'
        maxY      = maximum . map snd $ points'
        printPts  = forM_ [0..maxY] $ \row -> do
                      forM_ [0..maxX] $ \col ->
                        putChar (if (col, row) `elem` points' then '#' else ' ')
                      putStrLn ""

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/13_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn   "Part 2: "
  part2 input
  where parseInput str = (points, folds)
          where [pointStrs, foldStrs] = splitOn [""] . lines $ str
                points = map (unpack . P.parse point "") pointStrs
                folds  = map (unpack . P.parse fold "") foldStrs
                point  = (,) <$> (natNum <* comma) <*> natNum
                natNum = read <$> P.many1 P.digit
                comma  = P.char ','
                fold   = do P.string "fold along "
                            dim <- P.letter
                            P.char '='
                            charToDim dim <$> natNum

        unpack        = either (error . show) id
        charToDim 'x' = X
        charToDim 'y' = Y