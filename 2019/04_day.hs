pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

triples :: [a] -> [(a, a, a)]
triples xs = zip3 xs (tail xs) (tail $ tail xs)

pairOp :: (a -> b -> c) -> (a, b) -> c
pairOp op (x, y) = x `op` y

nonDecreasing :: (Ord a) => [a] -> Bool
nonDecreasing = all ((==True) . (pairOp (<=))) . pairs

twoSame :: (Eq a) => [a] -> Bool
twoSame = any ((==True) . (pairOp (==))) . pairs

strictTwoSame :: (Eq a) => [a] -> Bool
strictTwoSame xs = any (==(False, True, False)) $ triples $ (False:) $ (++[False]) $ map (pairOp (==)) $ pairs xs

solvePart1 :: Int
solvePart1 = length $ filter twoSame $ filter nonDecreasing $ map show [153517..630395]

solvePart2 :: Int
solvePart2 = length $ filter strictTwoSame $ filter nonDecreasing $ map show [153517..630395]