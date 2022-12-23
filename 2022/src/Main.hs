module Main where

import qualified Day01 ( solveDay )
import qualified Day02 ( solveDay )
import qualified Day03 ( solveDay )
import qualified Day04 ( solveDay )
import qualified Day05 ( solveDay )
import qualified Day06 ( solveDay )
import qualified Day07 ( solveDay )
import qualified Day08 ( solveDay )
import qualified Day09 ( solveDay )
import qualified Day10 ( solveDay )
import qualified Day11 ( solveDay )
import qualified Day12 ( solveDay )
import qualified Day13 ( solveDay )
import qualified Day14 ( solveDay )
import qualified Day15 ( solveDay )
import           Control.Monad ( zipWithM_ )
import           System.Environment ( getArgs )
import           Text.Read ( readMaybe )

main :: IO ()
main = do
  let solutions    = [ Day01.solveDay
                     , Day02.solveDay
                     , Day03.solveDay
                     , Day04.solveDay
                     , Day05.solveDay
                     , Day06.solveDay
                     , Day07.solveDay
                     , Day08.solveDay
                     , Day09.solveDay
                     , Day10.solveDay
                     , Day11.solveDay
                     , Day12.solveDay
                     , Day13.solveDay
                     , Day14.solveDay
                     , Day15.solveDay ]
  let printDay i s = putStrLn ("* Day " ++ show i) >> s >> putStrLn ""
  let printAll = zipWithM_ printDay [1 .. ] solutions

  -- TODO MaybeT
  args <- getArgs
  case length args of
    0 -> printAll
    _ -> case readMaybe (args !! 0) of
           Nothing -> putStrLn $ "Single argument should be a valid index!"
           Just i  -> if (i-1) > 0 && (i-1) < length solutions
                        then printDay i (solutions !! (i-1))
                        else putStrLn $ "Day " ++ show i ++ " not yet solved!"