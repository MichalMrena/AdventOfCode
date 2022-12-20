module Day13 where

import           Data.Maybe ( fromMaybe )
import           Data.List ( sort, intercalate, elemIndex )
import           Data.List.Split ( splitOn )
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

data PacketElem = List [PacketElem] | Num Int
type Packet = PacketElem

instance Show PacketElem where
  show (Num x) = show x
  show (List xs) = "[" ++ (intercalate "," . map show $ xs) ++ "]"

instance Eq PacketElem where
  (==) p1 p2 = compare p1 p2 == EQ

instance Ord PacketElem where
  compare = go
    where go (Num x) (Num y) = compare x y
          go (List []) (List (_:_)) = LT
          go (List (_:_)) (List []) = GT
          go (List []) (List []) = EQ
          go (List (x:xs)) (List (y:ys)) = case go x y of
                                            LT -> LT
                                            GT -> GT
                                            EQ -> go (List xs) (List ys)
          go (List xs) (Num y) = go (List xs) (List [Num y])
          go (Num x) (List ys) = go (List [Num x]) (List ys)

part1 :: [[Packet]] -> Int
part1 = sum . map fst . filter ((/=GT) . snd) . zip [1..]
      . map (uncurry compare)
      . map (\[a, b] -> (a, b))

part2 :: [[Packet]] -> Int
part2 ps = i1 * i2
  where
    sorted = sort . (div1 :) . (div2 :) . concat $ ps
    div1   = List [List [Num 2]]
    div2   = List [List [Num 6]]
    i1     = succ . fromMaybe undefined . elemIndex div1 $ sorted
    i2     = succ . fromMaybe undefined . elemIndex div2 $ sorted

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/13_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where
    parseInput  = map (map parsePacket) . splitOn [""] . lines
    parsePacket = unpack . P.parse packetElem ""
      where
        unpack       = either (error . show) id
        open         = P.char '['
        close        = P.char ']'
        comma        = P.char ','
        nat          = read <$> P.many1 P.digit
        packetNum    = Num <$> nat
        packetList   = List <$> (open *> (packetElem `P.sepBy` comma) <* close)
        packetElem   = packetList <|> packetNum