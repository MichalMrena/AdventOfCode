import Data.Char

input = ["<x=1, y=3, z=-11>", "<x=17, y=-10, z=-8>", "<x=-1, y=-15, z=2>", "<x=12, y=-4, z=-4>"]
-- input = ["<x=-1, y=0, z=2>", "<x=2, y=-10, z=-7>", "<x=4, y=-8, z=8>", "<x=3, y=5, z=-1>"]
-- input = ["<x=-8, y=-10, z=0>", "<x=5, y=5, z=10>", "<x=2, y=-7, z=3>", "<x=9, y=-8, z=-3>"]

type Positions  = [Int]
type Velocities = [Int]
type Position   = Int

isDigitOrSign :: Char -> Bool
isDigitOrSign c = any ($c) [(=='+'), (=='-'), isDigit]

extractNum :: String -> (Int, String)
extractNum s = let rest = dropWhile (not . isDigitOrSign) s
               in  (read $ takeWhile isDigitOrSign rest, dropWhile isDigitOrSign rest)

parsePoint :: String -> (Int, Int, Int)
parsePoint is = let (x, rx) = extractNum is
                    (y, ry) = extractNum rx
                    (z, rz) = extractNum ry
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

dimensionEnergy :: (Positions, Velocities) -> Int
dimensionEnergy (ps, vs) = (sum $ map abs ps) * (sum $ map abs vs)

solvePart1 :: Int
solvePart1 = let initVs = take 3 $ repeat (take (length input) $ repeat 0)
                 result = map (last . take 1001 . iterate simulateStep) $ (`zip` initVs) $ toList $ unzip3 $ map parsePoint input
                 ps     = map (map abs) $ map fst result
                 vs     = map (map abs) $ map snd result
                 pot    = zipWith3 (\ a b c -> a + b + c) (ps !! 0) (ps !! 1) (ps !! 2)
                 kin    = zipWith3 (\ a b c -> a + b + c) (vs !! 0) (vs !! 1) (vs !! 2)
             in  sum $ zipWith (*) pot kin