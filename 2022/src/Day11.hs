{-# LANGUAGE FlexibleContexts #-}

module Day11 where

import           Prelude hiding ( round )
import           Control.Monad.State
import           Data.List ( sortOn, foldl1' )
import           Data.IntMap.Strict ( IntMap, (!) )
import qualified Data.IntMap.Strict as M
import           Data.Sequence ( Seq, (|>) )
import qualified Data.Sequence as S
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

data Monkey = Monkey { op_       :: (Int -> Int)
                     , tester_   :: Int
                     , iftrue_   :: Int
                     , iffalse   :: Int
                     , inspects_ :: Int }
data GameState = GameState { items_   :: IntMap (Seq Int)
                           , monkeys_ :: IntMap Monkey }
type Item   = Int
type Relief = (Int -> Int)
data Expr   = Nat Int | Param deriving Show

incInspects :: Int -> Monkey -> Monkey
incInspects n m@(Monkey _ _ _ _ i) = m { inspects_ = i+n }

monkeyAction :: Relief -> Monkey -> Item -> State GameState ()
monkeyAction relief (Monkey op tester iftrue iffalse _) item = do
  let item' = relief . op $ item
  let targetIndex = if item' `mod` tester == 0 then iftrue else iffalse
  s@(GameState items _) <- get
  let items' = M.adjust (|> item') targetIndex items
  put $ s { items_ = items' }

turn :: Relief -> Int -> State GameState ()
turn relief monkeyIndex = do
  s@(GameState items monkeys) <- get
  let monkey      = monkeys ! monkeyIndex
  let monkeyItems = items ! monkeyIndex
  let items'      = M.adjust (const S.empty) monkeyIndex items
  let itemcount   = S.length monkeyItems
  let monkeys'    = M.adjust (incInspects itemcount) monkeyIndex monkeys
  put $ s { items_ = items', monkeys_ = monkeys' }
  sequence_ $ fmap (monkeyAction relief monkey) monkeyItems

round :: Relief -> State GameState ()
round relief = do (GameState is _) <- get
                  sequence_ $ map (turn relief) [0 .. (M.size is) - 1]

solve :: Relief -> Int -> GameState -> Int
solve relief numround initstate = i1 * i2
  where st = sequence_ (replicate numround (round relief))
        (_, finalstate@(GameState _ monkeys')) = runState st initstate
        (i1:i2:_) = sortOn negate . map (inspects_ . snd) . M.toList $ monkeys'

part1 :: GameState -> Int
part1 = solve (`div` 3) 20

part2 :: GameState -> Int
part2 s = solve relieve 10000 s
  where testers = map tester_ (map snd . M.toList . monkeys_ $ s)
        relieve = (`mod` (foldl1' lcm testers))

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/11_day.txt"
  putStrLn $ "Part 1: \n" ++ (show . part1 $ xs)
  putStrLn $ "Part 2: \n" ++ (show . part2 $ xs)
  where parseInput s = GameState (M.fromList itemslist) (M.fromList monkeylist)
          where tuples = unpack $ P.parse monkeys "" s
                itemslist  = zip [0..] (map (S.fromList . fst) tuples)
                monkeylist = zip [0..] (map snd tuples)

        unpack     = either (error . show) id
        nat        = read <$> P.many1 P.digit
        commaspace = P.string ", "
        space      = P.char ' '
        op         = P.char '*' *> pure (*)
                 <|> P.char '+' *> pure (+)
        param      = P.string "old" *> pure Param
        constant   = Nat <$> nat
        expr       = do e1 <- (param <|> constant)
                        space
                        o <- op
                        space
                        e2 <- (param <|> constant)
                        pure (e1, o, e2)
        monkeys    = monkey `P.sepBy` P.newline
        monkey     = do P.string "Monkey "
                        nat
                        P.char ':'
                        P.newline
                        P.string "  Starting items: "
                        items <- nat `P.sepBy` commaspace
                        P.newline
                        P.string "  Operation: new = "
                        op <- exprtof <$> expr
                        P.newline
                        P.string "  Test: divisible by "
                        tester <- nat
                        P.newline
                        P.string "    If true: throw to monkey "
                        iftrue <- nat
                        P.newline
                        P.string "    If false: throw to monkey "
                        iffalse <- nat
                        P.newline
                        pure $ (items, Monkey op tester iftrue iffalse 0)

        exprtof tup@(_, o, _) = case tup of
                                  (Nat x, _, Param) -> (x `o`)
                                  (Param, _, Nat x) -> (`o` x)
                                  (Param, _, Param) -> (\p -> o p p)