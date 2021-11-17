module Day10 where

import Control.Monad ( forM_ )
import Data.List.Split ( splitOn )
import Data.List ( foldl' )
import Data.Set ( fromList, member )

type Vec2 = (Int, Int)

vec2Plus :: Vec2 -> Vec2 -> Vec2
vec2Plus (lx, ly) (rx, ry) = (lx + rx, ly + ry)

rangeX, rangeY :: [Vec2] -> (Int, Int)
rangeX = foldl' (\(l, u) (x, _) -> (min x l, max x u)) (maxBound, minBound)
rangeY = foldl' (\(l, u) (_, y) -> (min y l, max y u)) (maxBound, minBound)

solve :: [Vec2] -> [Vec2] -> (Int, [Vec2])
solve vs = go 0 maxBound
  where
    height = abs . uncurry (-) . rangeY

    go :: Int -> Int -> [Vec2] -> (Int, [Vec2])
    go t h xs | h' > h    = (t, xs)
              | otherwise = go (t + 1) h' xs'
              where
                xs' = zipWith vec2Plus xs vs
                h'  = height xs'

printPoints :: [Vec2] -> IO ()
printPoints ps = do
  let set      = fromList ps
  let (lx, ux) = rangeX ps
  let (ly, uy) = rangeY ps

  forM_ [ly .. uy] (\y -> do
      forM_ [lx .. ux] (\x -> do
          putChar (if (x, y) `member` set then '*' else ' '))
      putStrLn "")

solveDay :: IO ()
solveDay = do
  (ps, vs) <- unzip . map parseLine . lines <$> readFile "input/10_day.txt"
  let (second, points) = solve vs ps

  putStrLn ("After " ++ show second ++ " seconds:")
  printPoints points

  where
    parseLine :: String -> (Vec2, Vec2)
    parseLine s = ((read px, read py), (read vx, read vy))
      where
        [ls, rs] = splitOn "> " s
        [px, py] = splitOn ", " $ drop 10 ls
        [vx, vy] = splitOn ", " $ drop 10 $ init rs