import Prelude hiding (Left, Right)
import Data.List   as L
import Data.Matrix as M
import Data.Vector (Vector)
import Data.List.Split
import Control.Applicative

type Pixels = M.Matrix Char
type SidePixels = Vector Char
data Tile = Tile { getId :: Int
                 , getPixels  :: [Pixels]
                 , getBorders :: [SidePixels] } deriving Show
data Side = Top | Right | Bottom | Left deriving Show
type TileRule = SidePixels -> Bool

instance Eq Tile where
    (Tile l _ _ ) == (Tile r _ _) = l == r

parseInput :: String -> [Tile]
parseInput = map (parseTile . lines) . splitOn "\n\n"
    where
        rotate          = map reverse . L.transpose
        flipHorizontal  = map reverse
        flipVertical    = reverse
        allOrientations ps = rotated ++ (map flipHorizontal rotated) ++ (map flipVertical rotated)
            where
                rotated = take 4 $ iterate rotate ps

        parseTile (idLine : pixelLines) = Tile tid allPixels allSides
            where
                tid       = read . init . drop 5 $ idLine
                getSides  = liftA2 side [Top, Right, Bottom, Left] . pure
                allPixels = map M.fromLists . allOrientations $ pixelLines
                allSides  = nub . concat . map (getSides) $ allPixels

        parseTile _ = error "Not good."

satisfies :: TileRule -> TileRule -> Tile -> (Bool, Int)
satisfies leftRule topRule t = case index of
                                   Nothing  -> (False, -1)
                                   (Just i) -> (True, i)
    where
        index       = findIndex checkPixels (getPixels t)
        checkPixels = \ps -> (leftRule . side Left $ ps) && (topRule . side Top $ ps)

findTile :: [Tile] -> TileRule -> TileRule -> (Tile, Int)
findTile ts r1 r2 = (\((_, i), t) -> (t, i))
                  . head
                  . dropWhile (not . fst . fst)
                  . zip (map (satisfies r1 r2) ts) $ ts

side :: Side -> Pixels -> SidePixels
side Top    ps = M.getRow 1 ps
side Bottom ps = M.getRow (nrows ps) ps
side Left   ps = M.getCol 1 ps
side Right  ps = M.getCol (ncols ps) ps

solvePart1 :: [Tile] -> Int
solvePart1 ts = product . map getId $ corners
    where
        allSides  = concat . map getBorders $ ts
        elemCount = \e -> length . filter ( == e)
        isEdge    = \b -> 1 == b `elemCount` allSides
        isCorner  = fst . satisfies isEdge isEdge
        corners   = filter isCorner $ ts

-- solvePart2 :: [Tile] -> Int
solvePart2 ts = M.flatten $ M.fromList 1 12 topRow
    where
        allSides  = concat . map getBorders $ ts
        elemCount = \e -> length . filter ( == e)
        isEdge    = \b -> 1 == b `elemCount` allSides

        pickPixels = \(t, i) -> (!! i) . getPixels $ t

        topLeft      = pickPixels $ findTile ts isEdge isEdge
        topRow       = topLeft : (reverse finishTopRow)
        finishTopRow = snd $ foldl pickTopTile (side Right topLeft, []) (take 11 . repeat $ ())
        pickTopTile  = \(prevSide, pss) _ -> let nextPs = pickPixels $ findTile ts (== prevSide) isEdge
                                             in (side Right nextPs, nextPs : pss)

        firstCol = []

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/20_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)