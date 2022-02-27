module Day17 where

import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

type Bounds = ((Int, Int), (Int, Int))

posX :: Int -> Int -> Int
posX dx t = let r = dx - t
            in div (dx^2 + dx) 2 - div (r^2 + r) 2

posY :: Int -> Int -> Int
posY dy t = 0

finalX :: Int -> Int
finalX dx = 0

-- Given initial velocity dx returns all t
-- for which (x, _) is in the target area.
validTs :: Bounds -> Int -> [Int]
validTs ((lhsX, rhsX), _) dx = []

-- Returns minimal initial velocity dx that can reach the target area.
getMinX :: Bounds -> Int
getMinX ((lhsX, _), _) = ceiling ((-1 + sqrt (fromIntegral (8 * lhsX + 1))) / 2)

-- part1 :: ((Int, Int), (Int, Int)) -> Int
part1 bounds@((lhsX, rhsX), (topY, bottomY)) =
      do dx <- [minDx .. maxDx]
         t <- validTs bounds dx
         dy <- [1 .. 100]
         return dx
  where minDx = getMinX bounds
        maxDx = rhsX

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