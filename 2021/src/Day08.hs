{-# LANGUAGE FlexibleContexts #-}
module Day08 where

import           Control.Monad ( void )
import           Data.Either ( fromRight )
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P

part1 :: [([String], [String])] -> Int
part1 = length . filter ((`elem` [2, 3, 4, 7]) . length) . concatMap snd

solveDay :: IO ()
solveDay = do
  ps <- parseInput <$> readFile "input/08_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ ps)
  where parseInput = map (unpack . P.parse line "") . lines
        unpack     = either (error . show) id
        line       = (,) <$> P.count 10 word <* symbol "|" <*> P.count 4 word
        word       = P.many1 P.letter <* P.spaces
        symbol s   = P.string s <* P.spaces
