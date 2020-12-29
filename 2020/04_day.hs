import Data.List
import Data.Maybe
import Data.List.Split
import Text.Read

type Field = (String, String)
type Passport = [Field]

parseInput :: String -> [Passport]
parseInput s = map (map parseField) ps
    where
        parseField = (\[key, val] -> (key, val)) . splitOn ":"
        ps         = map (concat . map words . lines) . splitOn "\n\n" $ s

hasReqFields :: Passport -> Bool
hasReqFields = (== (length requiredFields))
             . length
             . intersect requiredFields
             . map fst
    where
        requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

intInRange :: Int -> Int -> String -> Bool
intInRange lb ub intStr = let maybeNum = readMaybe intStr :: Maybe Int
                              num = fromJust maybeNum
                          in (isJust maybeNum) && (num >= lb && num <= ub)

isValidField :: Field -> Bool
isValidField ("byr", val) = intInRange 1920 2002 val
isValidField ("iyr", val) = intInRange 2010 2020 val
isValidField ("eyr", val) = intInRange 2020 2030 val
isValidField ("hgt", val) = let valLen = length val
                                unit   = drop (-2 + valLen) val
                                height = take (-2 + valLen) val
                            in case unit of
                                   "cm" -> intInRange 150 193 height
                                   "in" -> intInRange 59  76  height
                                   _    -> False
isValidField ("hcl", val) = let isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
                            in (7 == length val) && ('#' == head val) && (all isHexChar (tail val))
isValidField ("ecl", val) = val `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
isValidField ("pid", val) = (9 == length val) && (intInRange minBound maxBound val)
isValidField ("cid", _)   = True
isValidField _            = error "Not good."

solvePart1 :: [Passport] -> Int
solvePart1 = length
           . filter hasReqFields

solvePart2 :: [Passport] -> Int
solvePart2 = length
           . filter (all isValidField)
           . filter hasReqFields

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/04_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)