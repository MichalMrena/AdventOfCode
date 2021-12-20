{-# LANGUAGE FlexibleContexts #-}
module Day08 where

import           Control.Monad ( void, guard )
import           Data.Either ( fromRight )
import           Text.Parsec ( (<|>) )
import           Data.List ( sort, permutations )
import           Data.Map ( toList, fromList, (!) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P

part1 :: [([String], [String])] -> Int
part1 = length . filter ((`elem` [2, 3, 4, 7]) . length) . concatMap snd

part2 :: [([String], [String])] -> Int
part2 = sum . concatMap rowSum
  where ssds     = [ "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg"
                   , "abdefg", "acf", "abcdefg", "abcdfg" ]
        ssdToN   = fromList $ zip ssds [0..9]
        isDigit  = (`elem` ssds)
        decode m = sort . map (m ! )
        rowSum (alls, vs) = do
          perm <- permutations  "abcdefg"
          let mapping    = fromList $ zip perm ['a'..'g']
          let allDigits  = map (decode mapping) alls
          let digits     = map (decode mapping) vs
          guard (all isDigit allDigits)
          return $ foldl (\acc x -> 10 * acc + x) 0 (map (ssdToN ! ) digits)

solveDay :: IO ()
solveDay = do
  ps <- parseInput <$> readFile "input/08_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ ps)
  putStrLn $ "Part 2: " ++ (show . part2 $ ps)
  where parseInput = map (unpack . P.parse line "") . lines
        unpack     = either (error . show) id
        line       = (,) <$> P.count 10 word <* symbol "|" <*> P.count 4 word
        word       = P.many1 P.letter <* P.spaces
        symbol s   = P.string s <* P.spaces