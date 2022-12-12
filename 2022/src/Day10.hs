module Day10 where

import           Data.List ( foldl' )
import           Data.Vector ( (!) )
import qualified Data.Vector as V
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

import System.IO.Unsafe ( unsafePerformIO )

data Instrunction = Noop | Addx Int deriving Show

part1 :: [Instrunction] -> Int
part1 instuctions = sum (zipWith (*) is vs)
  where
        rs  = reverse $ scanr ($) 1 $ foldl' go [id] instuctions
        is  = takeWhile (< length rs) (20 : [60, 100 ..])
        vs  = map ((V.fromList rs) !) is
        go rs Noop     = id:rs
        go rs (Addx v) = (+v):id:rs

part2 :: [Instrunction] -> Int
part2 = const 2

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/10_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)

  where parseInput :: String -> [Instrunction]
        parseInput = unpack . P.parse (inst `P.sepBy1` P.newline) ""
        unpack     = either (error . show) id
        nat        = read  <$> P.many1 P.digit
        int        = (P.char '-' *> (negate <$> nat)) <|> nat
        inst       = (P.string "noop" >> pure Noop)
                 <|> (Addx <$> (P.string "addx " *> int))