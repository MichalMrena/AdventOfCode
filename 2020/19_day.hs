import Data.Bool
import Data.List.Split
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M

type RuleId = Int
data Rule   = Leaf Char
            | Sequence [RuleId]
            | Alternative Rule Rule deriving Show
type Rules = IntMap Rule

parseInput :: String -> (Rules, [String])
parseInput input = (M.fromList . map parseRuleLine $ rulesStr, messages)
    where
        [rulesStr, messages] = splitOn [""] . lines $ input
        parseRuleLine line   = (read i, parseRule . tail $ rest)
            where
                [i, rest] = splitOn ":" line

        parseRule str | '"' `elem` str = Leaf (str !! 1)
                      | '|' `elem` str = (\[l, r] -> Alternative (parseRule l) (parseRule r)) . splitOn "|" $ str
                      | otherwise      = Sequence . map read . words $ str

check :: Rules -> Rule -> String -> (Bool, String)
check _ (Leaf _) [] = (False, "")
check _ (Leaf c) cs = bool (False, cs) (True, tail cs) . (c == ) . head $ cs

check rules (Alternative l r) cs = if leftValid || rightValid
                                    then bool right left leftValid
                                    else (False, cs)
    where
        left@(leftValid, _)   = check rules l cs
        right@(rightValid, _) = check rules r cs

check rules (Sequence is) cs = foldl checkRule (True, cs) . map (rules ! ) $ is
    where
        checkRule (False, cs') _   = (False, cs')
        checkRule (True, cs') rule = check rules rule cs'

solvePart1 :: IntMap Rule -> [String] -> Int
solvePart1 rules = length . filter matches0
    where
        matches0 = (\(b, rs) -> b && null rs) . check rules (rules ! 0)

main :: IO ()
main = do
    (rules, messages) <- parseInput <$> readFile "./input/19_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 rules $ messages)