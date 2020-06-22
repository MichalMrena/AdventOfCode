import Data.Char
import qualified Data.Set as S

input = ["<x=1, y=3, z=-11>", "<x=17, y=-10, z=-8>", "<x=-1, y=-15, z=2>", "<x=12, y=-4, z=-4>"]

type Positions  = [Int]
type Velocities = [Int]
type Position   = Int

isDigitOrSign :: Char -> Bool
isDigitOrSign c = any ($c) [(=='+'), (=='-'), isDigit]

extractInt :: String -> (Int, String)
extractInt s = let rest = dropWhile (not . isDigitOrSign) s
                   ints = takeWhile isDigitOrSign rest
               in  (read $ ints, drop (length ints) rest)

parsePoint :: String -> (Int, Int, Int)
parsePoint is = let (x, rx) = extractInt is
                    (y, ry) = extractInt rx
                    (z, rz) = extractInt ry
                in  (x, y, z)

cmp :: (Ord a) => a -> a -> Int
cmp lhs rhs | lhs < rhs = -1
            | lhs > rhs = 1
            | otherwise = 0

toList :: (a, a, a) -> [a]
toList (xs, ys, zs) = [xs, ys, zs]

velocityChange :: Positions -> Position -> Int
velocityChange ps p = sum $ [cmp p' p | p' <- ps]

updateVelocities :: Positions -> Velocities -> Velocities
updateVelocities ps vs = zipWith (+) vs $ map (velocityChange ps) ps

simulateStep :: (Positions, Velocities) -> (Positions, Velocities)
simulateStep (ps, vs) = let newVs = updateVelocities ps vs
                        in  (zipWith (+) ps newVs, newVs)

initVs :: [[Int]]
initVs = take 3 $ repeat (take (length input) $ repeat 0)

solvePart1 :: Int
solvePart1 = let simResult           = map (last . take 1001 . iterate simulateStep) $ (`zip` initVs) $ toList $ unzip3 $ map parsePoint input
                 ps                  = map (map abs) $ map fst simResult
                 vs                  = map (map abs) $ map snd simResult
                 dimSum [xs, ys, zs] = zipWith3 (\ a b c -> a + b + c) xs ys zs
             in  sum $ zipWith (*) (dimSum ps) (dimSum vs)

firstLoopLength :: (Ord a) => S.Set a -> [a] -> Int
firstLoopLength ss (x:xs) | x `S.member` ss = S.size ss
                          | otherwise       = firstLoopLength (S.insert x ss) xs

solvePart2 = let simResult = map (map (\ (ps, vs) -> ps ++ vs)) $ map (iterate simulateStep) $ (`zip` initVs) $ toList $ unzip3 $ map parsePoint input
             in  foldl1 lcm $ map (firstLoopLength S.empty) $ simResult