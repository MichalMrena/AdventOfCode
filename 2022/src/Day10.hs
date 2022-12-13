module Day10 where

import           Data.List ( foldl', intercalate )
import           Data.List.Split ( chunksOf )
import           Data.Vector ( (!) )
import qualified Data.Vector as V
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

data Instrunction = Noop | Addx Int deriving Show

getvalues :: [Instrunction] -> [Int]
getvalues = tail . reverse . scanr ($) 1 . foldl' go [id]
  where go rs Noop     = id:rs
        go rs (Addx v) = (+v):id:rs

part1 :: [Instrunction] -> Int
part1 instuctions = sum (zipWith (*) is vs')
  where vs  = undefined : getvalues instuctions
        is  = takeWhile (< length vs) (20 : [60, 100 ..])
        vs' = map ((V.fromList vs) !) is
        go rs Noop     = id:rs
        go rs (Addx v) = (+v):id:rs

part2 :: [Instrunction] -> String
part2 instructions = intercalate "\n" $ chunksOf 40 cs
  where cs       = zipWith draw (getvalues instructions) (cycle [0..39])
        draw v i = if i `elem` [v-1, v, v+1] then '#' else ' '

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/10_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: \n" ++ (part2 $ xs)
  where parseInput :: String -> [Instrunction]
        parseInput = unpack . P.parse (inst `P.sepBy1` P.newline) ""
        unpack     = either (error . show) id
        nat        = read  <$> P.many1 P.digit
        int        = (P.char '-' *> (negate <$> nat)) <|> nat
        inst       = (P.string "noop" >> pure Noop)
                 <|> (Addx <$> (P.string "addx " *> int))