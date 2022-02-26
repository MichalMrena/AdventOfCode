module Day17 where

import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

posX :: Int -> Int -> Int
posX dx t = let r = dx - t
            in div (dx^2 + dx) 2 - div (r^2 + r) 2

posY :: Int -> Int -> Int
posY dy t = 0

finalX :: Int -> Int
finalX dx = 0

-- part1 :: ((Int, Int), (Int, Int)) -> Int
part1 ((lhsX, rhsX), (topY, bottomY)) = do dx <- [minDx .. maxDx]
                                           t <- [1 .. 100] -- validTs dx
                                           dy <- [1 .. 100]
                                           return dx
  where -- Minimal initial x-direction velocity that can reach the area.
        minDx = ceiling ((-1 + sqrt (fromIntegral (8 * lhsX + 1))) / 2)
        maxDx = rhsX

part2 :: String -> Int
part2 str = 0

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/17_day.txt"
  -- putStrLn $ "Part 1: " ++ (show . part1 $ input)
  -- putStrLn $ "Part 2: " ++ (show . part2 $ input)
  print input
  where parseInput = unpack . P.parse area "" :: String -> ((Int, Int), (Int, Int))
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