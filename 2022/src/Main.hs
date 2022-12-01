module Main where

import qualified Day01 ( solveDay )
import           Control.Monad ( zipWithM_ )

main :: IO ()
main = do
    let solutions    = [ Day01.solveDay ]
    let printDay i s = putStrLn ("* Day " ++ show i) >> s >> putStrLn ""
    zipWithM_ printDay [1 .. ] solutions