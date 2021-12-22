module Day10 where

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

        mkP c1 c2 = P.char c1 >> (P.eof
                              <|> void (P.char c2)
                              <|> void (P.many1 wrap >> (P.eof
                              <|> void (P.char c2))))

solveDay :: IO ()
solveDay = do
  ls <- lines <$> readFile "input/10_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ ls)