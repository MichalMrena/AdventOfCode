module Day10 where

import           Data.List ( sort, foldl' )
import           Control.Monad ( void )
import           Text.Parsec ( (<|>) )
import qualified Text.Parsec as P

part1 :: [String] -> Int
part1 = sum . map (charVal . badChar)
  where badChar str = case P.parse wrap "" str of
                        (Right _) -> '_'
                        (Left e)  -> str !! (P.sourceColumn (P.errorPos e) - 1)
        charVal '_' = 0
        charVal ')' = 3
        charVal ']' = 57
        charVal '}' = 1197
        charVal '>' = 25137

        -- Thank you Jason,
        -- https://twitter.com/lefticus/status/1466518147700199430
        braces        = mkP '(' ')'
        brackets      = mkP '[' ']'
        parenthesis   = mkP '{' '}'
        anglebrackets = mkP '<' '>'
        wrap          = braces <|> brackets <|> parenthesis <|> anglebrackets

        mkP c1 c2 = open >> (P.eof
                         <|> close
                         <|> (P.many1 wrap >> (P.eof <|> close)))
          where open  = void (P.char c1)
                close = void (P.char c2)

part2 :: [String] -> Integer
part2 = middle . sort . map (lineScore . map pair) . pickValid
  where
    pickValid = filter (not . null) . map isValid

    isValid = go []
      where go []       (c : cs)
              | c `elem` "({[<"  = go [c] cs
              | otherwise        = []

            go (s : stack) (c : cs)
              | pair s == c      = go stack cs
              | c `elem` "({[<"  = go (c : s : stack) cs
              | otherwise        = []

            go ls        []      = ls

    lineScore = foldl' (\acc c -> 5 * acc + charScore c) 0

    charScore ')' = 1
    charScore ']' = 2
    charScore '}' = 3
    charScore '>' = 4

    pair '(' = ')'
    pair '[' = ']'
    pair '{' = '}'
    pair '<' = '>'

    middle xs = go xs xs
      where go []           (y : ys) = y
            go [x]          (y : ys) = y
            go (_ : _ : xs) (_ : ys) = go xs ys

solveDay :: IO ()
solveDay = do
  ls <- lines <$> readFile "input/10_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ ls)
  putStrLn $ "Part 2: " ++ (show . part2 $ ls)