module Main where

import Day01 ( solveDay )
import Day02 ( solveDay )
import Day03 ( solveDay )
import Day04 ( solveDay )
import Day05 ( solveDay )
import Day06 ( solveDay )
import Control.Monad ( zipWithM_ )

main :: IO ()
main = do
    let solutions    = [ Day01.solveDay
                       , Day02.solveDay
                       , Day03.solveDay
                       , Day04.solveDay
                       , Day05.solveDay
                       , Day06.solveDay ]
    let is           = [1 .. length solutions]
    let printDay i s = do putStrLn ("* Day " ++ show i)
                          s
                          putStrLn ""
    zipWithM_ printDay is solutions