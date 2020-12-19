import Data.Bool
import Data.List.Split
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as M

type RuleId = Int
data Rule   = Leaf Char
            | Sequence [RuleId]
            | Alternative [Rule] deriving Show
type Rules = IntMap Rule

parseInput :: String -> (Rules, [String])
parseInput input = (M.fromList . map parseRuleLine $ rulesStr, messages)
    where
        [rulesStr, messages] = splitOn [""] . lines $ input
        parseRuleLine line   = (read i, parseRule . tail $ rest)
            where
                [i, rest] = splitOn ":" line

        parseRule str | '"' `elem` str = Leaf (str !! 1)
                      | '|' `elem` str = Alternative . map parseRule . splitOn "|" $ str
                      | otherwise      = Sequence . map read . words $ str

check :: Rules -> Rule -> String -> [String]
check _     (Leaf _)         [] = []
check _     (Leaf c)         cs = bool [] [tail cs] . (c == ) . head $ cs
check rules (Alternative rs) cs = rs >>= (\rule -> check rules rule cs)
check rules (Sequence is)    cs = foldl checkRule [cs] . map (rules ! ) $ is
    where
        checkRule css rule = css >>= check rules rule

solvePart1 :: IntMap Rule -> [String] -> Int
solvePart1 rules = length . filter (any null . check rules (rules ! 0))

solvePart2 :: IntMap Rule -> [String] -> Int
solvePart2 rules = solvePart1 rules'
    where
        new8   = Alternative [Sequence [42], Sequence [42, 8]]
        new11  = Alternative [Sequence [42, 31], Sequence [42, 11, 31]]
        rules' = M.insertWith const 8 new8
               . M.insertWith const 11 new11 $ rules

main :: IO ()
main = do
    (rules, messages) <- parseInput <$> readFile "./input/19_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 rules $ messages)
    putStrLn $ "Part 2: " ++ (show . solvePart2 rules $ messages)