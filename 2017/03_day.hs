import           Control.Monad.State
import           Data.Map ( Map )
import qualified Data.Map as M

type GridMap = Map (Int, Int) Int

spiral :: [(Int, Int)]
spiral = zip (go 0 1 0) (go 0 0 0)
  where
    go from to n
      | to >= 0 = [from, from + 1 .. to] ++ replicate n to
                                         ++ go to (from - 1) (n + 1)
      | to < 0  = [from, from - 1 .. to] ++ replicate n to
                                         ++ go to (from + 1) (n + 1)

part1 :: Int -> Int
part1 i = manDist (spiral !! (i - 1))
  where manDist (x, y) = abs x + abs y

part2 :: Int -> Int
part2 t = head . dropWhile (< t) $ xs
  where
    vals    = state <$> (cellVal <$> tail spiral)
    (xs, _) = runState (sequence vals) (M.singleton (0, 0) 1)
    cellVal p grid = (val, M.insert p val grid)
      where add (a, b) (x, y) = (a + x, b + y)
            adj = add <$> [p] <*> [ (-1, -1), (0, -1), (1, -1), (-1, 0)
                                  , (1, 0), (-1, 1), (0, 1), (1, 1) ]
            val = sum $ map (\p -> M.findWithDefault 0 p grid) adj

main :: IO ()
main = do
  p <- read <$> readFile "./input/03_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ p)
  putStrLn $ "Part 2: " ++ (show . part2 $ p)