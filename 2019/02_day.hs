import Data.List

initialXs = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0]

initParams :: Int -> Int -> [Int] -> [Int]
initParams p1 p2 (o:_:_:rest) = o:p1:p2:rest

setAt :: a -> Int -> [a] -> [a]
setAt element i xs = before ++ (element:(tail after))
    where
        (before, after) = splitAt i xs

op :: Int -> Int -> Int -> Int
op 1 a b = a + b
op 2 a b = a * b

runProgram :: Int -> [Int] -> [Int]
runProgram i xs 
    | 99 == opCode = xs
    | otherwise    = runProgram (i + 4) $ setAt opRes dsti xs
    where
        opCode = xs !! i
        lhs    = xs !! (xs !! (i + 1))
        rhs    = xs !! (xs !! (i + 2))
        dsti   = xs !! (i + 3)
        opRes  = op opCode lhs rhs

solvePart1 :: Int
solvePart1 = head $ runProgram 0 $ initParams 12 2 initialXs

solvePart2 :: Int
solvePart2 = let (v, n) = allPairs !! resultIndex in 100 * n + v
    where
        Just resultIndex = elemIndex 19690720 $ map solveFor allPairs
        solveFor (v, n) = head $ runProgram 0 $ initParams n v initialXs
        allPairs = [(v, n) | v <- [0..99], n <- [0..99]]