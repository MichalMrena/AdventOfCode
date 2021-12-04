{-# LANGUAGE FlexibleContexts #-}
module Day04 where

import qualified Text.Parsec as P
import           Control.Applicative ( Alternative((<|>)) )
import           Control.Monad ( void )

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/04_day.txt" :: IO ([Int], [[[Int]]])
  print input
  where parseInput str = case P.parse bingo "" str of
                           (Right x) -> x
                           (Left e)  -> error (show e)

        bingo      = (,) <$> (randoms <* P.skipMany P.space) <*> grids
        grids      = P.many1 grid
        grid       = P.count 5 gridLine <* eolOrEof
        gridLine   = P.count 5 (spaces >> naturalNum) <* eolOrEof
        randoms    = naturalNum `P.sepBy` P.char ','
        naturalNum = read <$> P.many1 P.digit
        spaces     = P.skipMany (P.char ' ')
        eolOrEof   = void P.newline <|> P.eof