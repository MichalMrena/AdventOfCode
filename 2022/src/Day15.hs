module Day15 where

import           Data.Maybe ( fromMaybe )
import qualified Data.IntMap as M
import           Data.List ( foldl', foldl1', sortOn, nub, find )
import qualified Data.Set as S
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

type Point        = (Int, Int)
type Pair         = (Point, Point)
type Interval     = (Int, Int)
type IntervalList = [Interval]

insert :: IntervalList -> Interval -> IntervalList
insert [] i = [i]
insert list interval = go interval list
  where
    overlap  (l1, r1) (l2, r2) = l1 <= r2 && r1 >= l2
    touch    (l1, r1) (l2, r2) = (abs (r1 - l2) == 1) || (abs (r2 - l1) == 1)
    union    (l1, r1) (l2, r2) = (min l1 l2, max r1 r2)
    isBefore (l1, _) (l2, _)   = l1 < l2

    go new [] = [new]
    go new (old:is)
      | new `overlap` old  = go (union new old) is
      | new `touch` old    = go (union new old) is
      | new `isBefore` old = new : old : is
      | otherwise          = old : go new is

cantContainsInRow :: [Pair] -> Int -> IntervalList
cantContainsInRow ps y = foldl' (\acc x -> insert acc (x,x)) intervals occupXs
  where
    occupXs = fromMaybe [] $ M.lookup y ytox
    intervals = foldl' insert [] . concatMap (cantContain y) $ ps
    ytox = foldl' (\acc (x, y) -> M.insertWith (++) y [x] acc) M.empty occupied
    occupied = (map fst ps) ++ (nub . map snd $ ps)

    taxidist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

    cantContain y (s@(sx, sy), b@(bx, by))
      | disttoy > radius = []
      | otherwise        = [(sx - xspan, sx + xspan)]
      where radius  = taxidist s b
            disttoy = abs (sy - y)
            xspan   = radius - disttoy

part1 :: [Pair] -> Int
part1 ps = subtract numOccupied . sum . map rangeSize $ intervals
  where
    theY        = 2000000
    occupXs     = map fst . filter ((==theY) . snd) . nub . map snd $ ps
    numOccupied = length $ filter (==True) $ contains <$> intervals <*> occupXs
    intervals   = cantContainsInRow ps theY

    rangeSize (l, r)  = r - l + 1
    contains (l, r) x = x >= l && x <= r

part2 :: [Pair] -> Int
part2 ps = case theRow of
             Nothing -> undefined
             Just (y, [(_, r1), _]) -> 4000000 * (r1 + 1) + y
  where intervals = zip [0..] $ map (cantContainsInRow ps) [0 .. 4000000]
        theRow    = find ((==2) . length . snd) intervals

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/15_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  where parseInput = unpack . P.parse pairs ""
        unpack     = either (error . show) id
        nat        = read <$> P.many1 P.digit
        int        = (P.char '-' *> (negate <$> nat)) <|> nat
        pairs = pair `P.sepBy` P.newline
        pair  = (,) <$> (P.string "Sensor at " *> point)
                    <*> (P.string ": closest beacon is at " *> point)
        point = (,) <$> (P.string "x=" *> int) <*> (P.string ", y=" *> int)