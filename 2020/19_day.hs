import Data.Bool
import Data.List
import Data.List.Split
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M

type RuleId = Int
data Rule   = Leaf Char
            | Inner [RuleId]
            | InnerAlt Rule Rule deriving Show
type Rules = IntMap Rule

parseInput :: String -> (Rules, [String])
parseInput input = (M.fromList . map parseRuleLine $ rulesStr, messages)
    where
        [rulesStr, messages] = splitOn [""] . lines $ input
        parseRuleLine line   = (read i, parseRule . tail $ rest)
            where
                [i, rest] = splitOn ":" line

        parseRule str | '"' `elem` str = Leaf (str !! 1)
                      | '|' `elem` str = (\[l, r] -> InnerAlt (parseRule l) (parseRule r)) . splitOn "|" $ str
                      | otherwise      = Inner . map read . words $ str

eat :: Char -> String -> String
eat c cs = bool cs (tail cs) ((== c) . head $ cs)

check :: Rules -> Rule -> String -> [String]
check _     (Leaf c)       rest = pure . eat c $ rest
check rules (InnerAlt l r) rest = nub $ (check rules l rest) ++ (check rules r rest)
check rules (Inner is)     rest = nub . foldl checkRule [rest] $ innerRules
    where
        checkRule  = (\rests rule -> rests >>= check rules rule)
        innerRules = map (rules ! ) is

solvePart1 :: IntMap Rule -> [String] -> Int
solvePart1 rules = length . filter matches0
    where
        matches0 m = [] `elem` check rules (rules ! 0) m

solve :: IO ()
solve = do
    (rules, messages) <- parseInput <$> readFile "./input/19_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 rules $ messages)

main = solve