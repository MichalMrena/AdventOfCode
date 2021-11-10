module Day08 where

import           Prelude hiding (Ordering(..))
import qualified Data.Map as M
import           Data.Char ( isDigit )

type Register    = String
data Expr        = Constant Int | Variable String deriving Show
data Rel         = LT | GT | LTE | GTE | EQ | NEQ deriving Show
data Condition   = Condition Expr Rel Expr deriving Show
data Instruction = Instruction Register Int Condition deriving Show

runProgram :: [Instruction] -> (Int, M.Map String Int)
runProgram = foldl exec (0, M.empty)
  where
    exec (gMax, rs) (Instruction register x condition) =
      if holds condition rs
        then let newVal = x + M.findWithDefault 0 register rs
                 rs'    = M.insert register newVal rs
             in (max newVal gMax, rs')
        else (gMax, rs)

    holds (Condition lhs rel rhs) rs =
      evalCondition (evalExpr lhs rs) rel (evalExpr rhs rs)

    evalExpr (Constant x) _  = x
    evalExpr (Variable n) rs = M.findWithDefault 0 n rs

    evalCondition lhs LT  rhs = lhs <  rhs
    evalCondition lhs LTE rhs = lhs <= rhs
    evalCondition lhs GT  rhs = lhs >  rhs
    evalCondition lhs GTE rhs = lhs >= rhs
    evalCondition lhs EQ  rhs = lhs == rhs
    evalCondition lhs NEQ rhs = lhs /= rhs

part1 :: [Instruction] -> Int
part1 = maximum . map snd . M.toList . snd . runProgram

part2 :: [Instruction] -> Int
part2 = fst . runProgram

solveDay :: IO ()
solveDay = do
  program <- map (readLine . words) . lines <$> readFile "input/08_day.txt"
  putStrLn $ "Part 1: " ++ show (part1 program)
  putStrLn $ "Part 2: " ++ show (part2 program)
  where
    readLine [dst, s, op, _, lhs, rel, rhs] =
      Instruction dst (readOp s op) cond
      where cond = Condition (readExpr lhs) (readRel rel) (readExpr rhs)

    readOp "inc" x = read x
    readOp "dec" x = negate (read x)

    readExpr str@(c : _) | isDigit c || c == '-' = Constant (read str)
                         | otherwise             = Variable str

    readRel "<"  = LT
    readRel "<=" = LTE
    readRel ">"  = GT
    readRel ">=" = GTE
    readRel "==" = EQ
    readRel "!=" = NEQ