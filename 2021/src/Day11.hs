{-# LANGUAGE TupleSections #-}
module Day11 where

import           Data.List ( foldl' )
import           Data.Char ( ord )
import           Data.Map ( Map, (!) )
import qualified Data.Map as M

type Energy  = Int
type Counter = Int
type Grid    = Map (Int, Int) Cell
data Cell    = Cell { energy_ :: Int, counter_ :: Int, mark_ :: Bool }

step :: Grid -> Grid
step grid = reset <$> go (unmark <$> grid') flashable
    where
      grid'     = fmap incEnergy grid
      flashable = map fst . filter ((>9) . energy_ . snd) $ M.toList grid'

      reset       (Cell e c f) = Cell (if f then 0 else e) c f
      incEnergy   (Cell e c f) = Cell (e + 1) c f
      resetEnergy (Cell e c f) = Cell 0 c f
      incCounter  (Cell e c f) = Cell e (c + 1) f
      mark        (Cell e c f) = Cell e c True
      unmark      (Cell e c f) = Cell e c False

      neis (r, c) = filter (\(a, b) -> inRange a && inRange b) cs
        where inRange x = x >=0 && x <= 9
              cs = [ (r + dr, c + dc) | dr <- [-1..1]
                                      , dc <- [-1..1]
                                      , (0,0) /= (dr, dc) ]

      go m []           = m
      go m (p : fs)
        | mark_ (m ! p) = go m fs
        | otherwise     = go (M.adjust (incCounter . mark) p m') fs'
        where m'  = foldl' (flip (M.adjust incEnergy)) m (neis p)
              fs' = filter ((>9) . energy_ . (m' !)) (neis p) ++ fs

part1 :: Grid -> Int
part1 = countFlashes . (!! 100) . iterate step
  where countFlashes = sum . map (counter_ . snd) . M.toList

part2 :: Grid -> Int
part2 = length . takeWhile (not . allFlashed) . iterate step
  where allFlashed = all (mark_ . snd) . M.toList

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/11_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where parseInput = M.fromList . zip is . concatMap parseLine . lines
        parseLine  = map (mkCell . subtract (ord '0') . ord)
        is         = (,) <$> [0..9] <*> [0..9]
        mkCell e   = Cell e 0 False