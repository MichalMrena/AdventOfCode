module Main where

import qualified Day01 ( solveDay )
import qualified Day02 ( solveDay )
import qualified Day03 ( solveDay )
import qualified Day04 ( solveDay )
import           Control.Monad ( zipWithM_ )

main :: IO ()
main = do
    let solutions    = [ Day01.solveDay
                       , Day02.solveDay
                       , Day03.solveDay
                       , Day04.solveDay ]
    let printDay i s = putStrLn ("* Day " ++ show i) >> s >> putStrLn ""
    zipWithM_ printDay [1 .. ] solutions