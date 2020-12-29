import Control.Applicative
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Tile = Black | White deriving (Show, Eq)
data Move = East | West | Southeast | Northeast | Southwest | Northwest deriving (Show, Enum, Bounded)

diff :: Move -> (Int, Int)
diff East      = (1, 1)
diff West      = (-1, -1)
diff Southeast = (1, -1)
diff Northeast = (0, 2)
diff Southwest = (0, -2)
diff Northwest = (-1, 1)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

parseInput :: String -> [[Move]]
parseInput = map parseLine . lines
    where
        parseLine ('e' : cs)       = East      : parseLine cs
        parseLine ('s' : 'e' : cs) = Southeast : parseLine cs
        parseLine ('s' : 'w' : cs) = Southwest : parseLine cs
        parseLine ('w' : cs)       = West      : parseLine cs
        parseLine ('n' : 'w' : cs) = Northwest : parseLine cs
        parseLine ('n' : 'e' : cs) = Northeast : parseLine cs
        parseLine _                = []

initPositions :: [[Move]] -> S.Set (Int, Int)
initPositions = S.fromList
              . map fst
              . filter ((== Black) . snd)
              . M.toList
              . foldl' ft (M.singleton (0, 0) White)
              . map (map diff)
    where
        flipTile _ Black = White
        flipTile _ White = Black
        ft m ds = (\p -> M.insertWith flipTile p Black m) . foldl' addPoints (0, 0) $ ds

solvePart1 :: [[Move]] -> Int
solvePart1 = S.size . initPositions

solvePart2 :: [[Move]] -> Int
solvePart2 = S.size . (!! 100) . iterate live . initPositions
    where
        live bs = (S.filter turnBlack ws) `S.union` (S.filter stayBlack bs)
            where
                ds               = map diff [minBound .. maxBound]
                neighbours p     = S.fromList . liftA2 addPoints ds $ [p]
                neighbourCount p = S.size . S.intersection bs . neighbours $ p
                stayBlack        = (`elem` [1, 2]) . neighbourCount
                turnBlack        = (== 2) . neighbourCount
                ws               = foldl' S.union S.empty . S.map neighbours $ bs

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/24_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)