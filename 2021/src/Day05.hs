{-# LANGUAGE FlexibleContexts #-}
module Day05 where

import qualified Text.Parsec as P

type Point = (Int, Int)
type Line = (Point, Point)

solveDay :: IO ()
solveDay = do
    lines <- readInput <$> readFile "input/05_day.txt" :: IO [Line]
    print lines
    putStrLn "done"
    where readInput str = case P.parse lines "" str of
                            (Right ls) -> ls
                            (Left e)   -> error (show e)
          lines         = line `P.endBy` P.newline
          line          = (,) <$> (point <* P.string " -> ") <*> point
          point         = (,) <$> (naturalNum <* comma) <*> naturalNum
          naturalNum    = read <$> P.many1 P.digit
          comma         = P.char ','