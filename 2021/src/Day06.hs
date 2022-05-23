{-# LANGUAGE FlexibleContexts #-}
module Day06 where

import qualified Text.Parsec as P

count :: Int -> Int
count days | days < 0  = 0
           | otherwise = 1 + sum (map count $ takeWhile (>0) children)
  where children = (days - 8) : [(days - 8) - 7 * i | i <- [1 ..]]

solveDay :: IO ()
solveDay = do
  ns <- readNums <$> readFile "input/06_day.txt" :: IO [Int]
  print ns
  print (sum $ map (count  . (19 -)) ns)
  putStrLn "done"
  where readNums str = case P.parse nums "" str of
                         (Right ns) -> ns
                         (Left e)   -> error (show e)
        nums = map read <$> P.many1 P.digit `P.sepBy` P.char ','