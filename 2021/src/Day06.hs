{-# LANGUAGE FlexibleContexts #-}
module Day06 where

import qualified Text.Parsec as P

count :: Int -> Int
count days = 0

solveDay :: IO ()
solveDay = do
  ns <- readNums <$> readFile "input/06_day.txt" :: IO [Int]
  print ns
  putStrLn "done"
  where readNums str = case P.parse nums "" str of
                         (Right ns) -> ns
                         (Left e)   -> error (show e)
        nums = map read <$> P.many1 P.digit `P.sepBy` P.char ','