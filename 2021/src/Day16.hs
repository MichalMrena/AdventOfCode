module Day16 where

import           Data.List ( foldl' )
import qualified Text.Parsec as P

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

-- parseLiteral :: String -> PacketBody
-- parseLiteral = Literal . binToDec . go
--   where go ('0' : bs) = take 4 bs
--         go ('1' : bs) = take 4 bs ++ go (drop 4 bs)

-- parseOperator :: String -> PacketBody
-- parseOperator (lenTypId : bits) = Literal 0
--   where x = 1

-- parseBody :: Int -> String -> PacketBody
-- parseBody typeId = if typeId == 4 then parseLiteral else parseOperator

-- parsePacket :: String -> Packet
-- parsePacket bits = Packet version typeId body
--   where version = binToDec . take 3 $ bits
--         typeId  = binToDec . take 3 . drop 3 $ bits
--         body    = parseBody typeId (drop 6 bits)

-- TODO will be better with parsec

part1 :: String -> Int
part1 str = 0

part2 :: String -> Int
part2 str = 0

solveDay :: IO ()
solveDay = do
  input <- readFile "input/16_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ input)
  putStrLn $ "Part 2: " ++ (show . part2 $ input)
  print input