module Day07 where

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)


import qualified Data.Map as M
import qualified Data.Set as S


-- alias Parsec.parse for more concise usage in my examples:
parse rule text = Parsec.parse rule "(source)" text


data Program = Program Int [String] deriving Show

solveDay :: IO ()
solveDay = do
    xs <- parse <$> readFile "input/07_day.txt"
    putStrLn "done"

    where
        parseLine m s = M.empty
        parse = foldl parseLine M.empty . lines