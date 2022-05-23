module Day17 where

import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

type Bounds = ((Int, Int), (Int, Int))

posX :: Int -> Int -> Int
posX dx t = div (-t^2 + (2 * dx + 1) * t) 2

posY :: Int -> Int -> Int
posY dy t = 0

finalX :: Int -> Int
finalX dx = 0

-- Given initial velocity dx returns all t
-- for which (x, _) is in the target area.

-- Find ts such that:
-- posX dx t >= lhsX && posX dx t <= rhx && t <= dx

validTs :: Bounds -> Int -> [Int]
validTs ((lhsX, rhsX), _) dx
  | l2 <= r1 || l1 >= r2 = [floor l1 .. ceiling l2]
  | l1 < r1 && l2 > r1   = [floor l1 .. ceiling r1]
  | l1 < r2 && l2 > r2   = [floor r2 .. ceiling l2]
  | otherwise            = []
  where
      d = fromIntegral dx :: Double
      l = fromIntegral lhsX :: Double
      r = fromIntegral rhsX :: Double
      l1 = (2 * d + 1 - sqrt (4 * d ^ 2 + 4 * d + 1 - 8 * l)) / 2
      r1 = (2 * d + 1 + sqrt (4 * d ^ 2 + 4 * d + 1 - 8 * l)) / 2
      l2 = (2 * d + 1 - sqrt (4 * d ^ 2 + 4 * d + 1 - 8 * r)) / 2
      r2 = (2 * d + 1 + sqrt (4 * d ^ 2 + 4 * d + 1 - 8 * r)) / 2

validYs :: Bounds -> Int -> Int -> [Int]
validYs (_, (topY, bottomY)) dx t = [1 .. 100]

-- Returns minimal initial velocity dx that can reach the target area.
getMinX :: Bounds -> Int
getMinX ((lhsX, _), _) = ceiling ((-1 + sqrt (fromIntegral (8 * lhsX + 1))) / 2)

getMaxX :: Bounds -> Int
getMaxX ((_, rhsX), _) = rhsX

-- part1 :: ((Int, Int), (Int, Int)) -> Int
part1 bounds@((lhsX, rhsX), (topY, bottomY)) =
      do dx <- [minDx .. maxDx]
         t <- validTs bounds dx
         dy <- validYs bounds dx t
         return dx
  where minDx = getMinX bounds
        maxDx = getMaxX bounds

parseInput :: String -> Bounds
parseInput = unpack . P.parse area "" :: String -> Bounds
      where
        unpack     = either (error . show) id
        area       = do P.string "target area: x="
                        xRange <- range
                        P.string ", y="
                        yRange <- range
                        return (xRange, yRange)
        int        = read <$> P.many1 P.digit
                    <|>
                    negate . read <$> (P.char '-' *> P.many1 P.digit)
        range      = (,) <$> (int <* P.string "..") <*> int

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/17_day.txt"
  -- putStrLn $ "Part 1: " ++ (show . part1 $ input)
  -- putStrLn $ "Part 2: " ++ (show . part2 $ input)
  print input