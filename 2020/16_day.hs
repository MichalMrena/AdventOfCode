import Data.List
import Data.List.Split

data Range  = Range Int Int deriving Show
data Field  = Field String [Range] deriving Show
type Ticket = [Int]

instance Eq Field where
    (Field n1 _) == (Field n2 _) = n1 == n2

parseInput :: String -> ([Field], Ticket, [Ticket])
parseInput str = (fields, myTicket, otherTickets)
    where
        [part1, part2, part3] = map lines . splitOn "\n\n" $ str
        myTicket              = read ("[" ++ (part2 !! 1) ++ "]")
        otherTickets          = map (read . ('[' :) . (++ "]")) . tail $ part3
        fields                = map parseFieldLine part1
        parseFieldLine s      = let [name, rulesStr]    = splitOn ":" s
                                    [first, _, second]  = words rulesStr
                                    parseRule ruleStr   = strToRule $ splitOn "-" ruleStr
                                    strToRule [l, u]    = Range (read l) (read u)
                                    strToRule _         = error "Shut up linter."
                                in Field name [parseRule first, parseRule second]

inRange :: Range -> Int -> Bool
inRange (Range l u) x = x >= l && x <= u

matchesFiled :: Int -> Field -> Bool
matchesFiled x (Field _ [r1, r2]) = (inRange r1 x) || (inRange r2 x)
matchesFiled _ _                  = error "Shut up linter."

solvePart1 :: ([Field], Ticket, [Ticket]) -> Int
solvePart1 (fields, _, otherTickets) = sum
                                     . filter (\t -> not . any (matchesFiled t) $ fields)
                                     . concat $ otherTickets

solvePart2 :: ([Field], Ticket, [Ticket]) -> Int
solvePart2 (fields, myTicket, otherTickets) = product
                                            . map fst $ filter (("departure" `isPrefixOf`) . snd)
                                            . zip myTicket $ fieldsOrdered
    where
        isValidTicket      = all (\x -> any (matchesFiled x) fields)
        filterFields ts    = filter (\f -> all (flip matchesFiled $ f) ts) fields
        validTickets       = filter isValidTicket otherTickets
        filteredFields     = map filterFields . transpose $ validTickets
        filteredFieldNames = map (map (\(Field name _) -> name)) filteredFields
        fieldsOrdered      = map snd
                           . sortOn fst
                           . sieve $ filteredFieldNames
        sieve :: [[String]] -> [(Int, String)]
        sieve xxs | all null xxs = []
                  | otherwise    = (i, name) : sieve xxs'
            where
                (i, [name]) = head . dropWhile ((/= 1) . length . snd) . zip [0 .. ] $ xxs
                xxs'        = map (filter (/= name)) xxs

solve :: IO ()
solve = do
    input <- parseInput <$> readFile "./input/16_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)