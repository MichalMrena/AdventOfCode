module Day16 where

import           Data.Bool ( bool )
import           Data.List ( foldl' )
import qualified Text.Parsec as P
import qualified Data.Functor.Identity as Id

data PacketBody = Literal Int
                | Operator [Packet] deriving Show

data Packet = Packet { version_ :: Int
                     , typId_   :: Int
                     , body_    :: PacketBody } deriving Show

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"

binToDec :: String -> Int
binToDec = foldl' (\acc b -> 2 * acc + ctoi b) 0
  where ctoi '0' = 0
        ctoi '1' = 1

literalPacket :: P.ParsecT String u Id.Identity PacketBody
literalPacket = Literal . binToDec <$> go
  where go = do b <- P.digit
                if b == '0' then P.count 4 P.digit
                            else do part <- P.count 4 P.digit
                                    rest <- go
                                    return (part ++ rest)

operatorPacket :: P.ParsecT String u Id.Identity PacketBody
operatorPacket = do lenTypeId <- P.digit
                    if lenTypeId == '0'
                      then do bitLen <- binToDec <$> P.count 15 P.digit
                              bits <- P.count bitLen P.digit
                              return (Operator (parseManyPackets bits))
                      else do subPacketCount <- binToDec <$> P.count 11 P.digit
                              Operator <$> P.count subPacketCount packet
  where parseManyPackets = unpack . P.parse (P.many packet) ""
        unpack = either (error . show) id

packetBody :: Int -> P.ParsecT String u Id.Identity PacketBody
packetBody typeId = if typeId == 4 then literalPacket else operatorPacket

packet :: P.ParsecT String u Id.Identity Packet
packet = do version <- binToDec <$> P.count 3 P.digit
            typeId  <- binToDec <$> P.count 3 P.digit
            body    <- packetBody typeId
            return $ Packet version typeId body

parsePacket :: String -> Packet
parsePacket = unpack . P.parse packet "" . concatMap hexToBin
  where unpack = either (error . show) id

part1 :: String -> Int
part1 = go . parsePacket
  where go (Packet v _ (Literal _)) = v
        go (Packet v _ (Operator ps)) = v + sum (map go ps)

part2 :: String -> Int
part2 = go . parsePacket
  where go (Packet _ _ (Literal v)) = v
        go (Packet _ 0 (Operator ps)) = sum (map go ps)
        go (Packet _ 1 (Operator ps)) = product (map go ps)
        go (Packet _ 2 (Operator ps)) = minimum (map go ps)
        go (Packet _ 3 (Operator ps)) = maximum (map go ps)
        go (Packet _ 5 (Operator [p1, p2])) = bool 0 1 (go p1 > go p2)
        go (Packet _ 6 (Operator [p1, p2])) = bool 0 1 (go p1 < go p2)
        go (Packet _ 7 (Operator [p1, p2])) = bool 0 1 (go p1 == go p2)

solveDay :: IO ()
solveDay = do
  input <- readFile "input/16_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)