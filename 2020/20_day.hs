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

allOrientations :: (Eq a) => [[a]] -> [[[a]]]
allOrientations ps = nub $ (rotate4 ps) ++ (rotate4 . flipVertical $ ps) ++ (rotate4 . flipHorizontal $ ps)
    where
        rotate4         = take 4 . iterate rotate
        rotate          = map reverse . L.transpose
        flipHorizontal  = map reverse
        flipVertical    = reverse

parseInput :: String -> [Tile]
parseInput = map (parseTile . lines) . splitOn "\n\n"
    where
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

findTile :: [Tile] -> [Int] -> TileRule -> TileRule -> (Tile, Int)
findTile ts neiIds r1 r2 = (\((_, i), t) -> (t, i))
                         . head
                         . dropWhile (\((sat, _), t) -> (not sat) || ((getId t) `elem` neiIds))
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

solvePart2 :: [Tile] -> Int
solvePart2 ts = gridCount
    where
        elemCount      = \e -> length . filter ( == e)
        allSides       = concat . map getBorders $ ts
        edges          = filter (\b -> 1 == b `elemCount` allSides) allSides
        isEdge         = (`elem` edges)
        pickPixels     = \(t, i) -> (!! i) . getPixels $ t
        topLeft        = findTile ts [] isEdge isEdge

        pickBottom r c = side Bottom $ pickPixels $ gridStore !! (r - 1) !! c
        pickRight  r c = side Right  $ pickPixels $ gridStore !! r !! (c - 1)
        pickNeiId  r c = getId $ fst $ gridStore !! r !! c

        gridStore      = map (\x -> map (grid' x) [0 .. ]) [0 .. ]
        grid' 0 0      = topLeft
        grid' 0 c      = findTile ts [pickNeiId 0 (c - 1)]                      (== (pickRight 0 c)) isEdge
        grid' r 0      = findTile ts [pickNeiId (r - 1) 0]                      isEdge               (== (pickBottom r 0))
        grid' r c      = findTile ts [pickNeiId (r - 1) c, pickNeiId r (c - 1)] (== (pickRight r c)) (== (pickBottom r c))

        dropBorders    = M.submatrix 2 9 2 9
        image          = M.flatten $ M.fromLists $ map (map dropBorders) $ map (map pickPixels) $ map (take 12) $ take 12 gridStore
        images         = map M.fromLists $ allOrientations $ M.toLists image
        subimages img  = map M.toList $ (\sr sc -> M.submatrix sr (sr + 2) sc (sc + 21) img) <$> [1 .. 94] <*> [1 .. 75]
        seamonster     = "..................#...#....##....##....###...#..#..#..#..#..#....."
        isSeamonster   = and . map (uncurry (==)) . filter ((== '#') . fst) . zip seamonster
        rightImage     = head . dropWhile (not . any isSeamonster . subimages) $ images
        monsterCount   = length . filter isSeamonster . subimages $ rightImage
        gridCount      = (length . filter (== '#') . M.toList $ rightImage) - 15 * monsterCount

main :: IO ()
main = do
    input <- parseInput <$> readFile "./input/20_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ input)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ input)