import Data.Char
import Data.List

data Element = Const Int | Add | Mul | Bracket [Element] deriving Eq

instance Show Element where
    show (Const i)    = show i
    show Add          = "+"
    show Mul          = "*"
    show (Bracket es) = "(" ++ (concat . map show $ es) ++ ")"

getVal :: Element -> Int
getVal (Const x) = x
getVal _         = error "Invalid element."

parseInput :: String -> [[Element]]
parseInput = map parse . lines . filter (/= ' ')
    where
        parse ('+' : cs) = Add : parse cs
        parse ('*' : cs) = Mul : parse cs
        parse ('(' : cs) = let (bcs, cs') = extractBracket cs in (Bracket $ parse bcs) : parse cs'
        parse (c   : cs) = (ctoi c) : parse cs
        parse []         = []

        ctoi = Const . (subtract . ord $ '0') . ord

        extractBracket cs = (take (len - 1) cs, drop len cs)
            where
                len = length . takeWhile ((> 0) . snd) . zip cs . scanl processChar (1 :: Int) $ cs
                    where
                        processChar n '(' = succ n
                        processChar n ')' = pred n
                        processChar n _   = n

justLeftEval :: [Element] -> Element
justLeftEval = head . evalInner
    where
        evalInner []  = []
        evalInner [c] = [c]
        evalInner ((Const x) : Add : (Const y) : es) = evalInner $ (Const (x + y)) : es
        evalInner ((Const x) : Mul : (Const y) : es) = evalInner $ (Const (x * y)) : es
        evalInner s = error $ "Not good." ++ (show s)

precedenceEval :: [Element] -> Element
precedenceEval es = case Mul `elemIndex` es of
                        Nothing  -> justLeftEval es
                        (Just i) -> let (les, res) = splitAt i es
                                    in justLeftEval [precedenceEval les, Mul, precedenceEval . tail $ res]

evalExpression :: ([Element] -> Element) -> [Element] -> Element
evalExpression evaluator = evaluator . map flatten
    where
        flatten (Bracket es') = evalExpression evaluator es'
        flatten t             = t

solvePart1 :: [[Element]] -> Int
solvePart1 = sum . map (getVal . evalExpression justLeftEval)

solvePart2 :: [[Element]] -> Int
solvePart2 = sum . map (getVal . evalExpression precedenceEval)

solve :: IO ()
solve = do
    input <- parseInput <$> readFile "./input/18_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 1: " ++ (show . solvePart2 $ input)