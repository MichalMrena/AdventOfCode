import Data.Bits
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

data Instruction = Mask Int Int Int | Mem Int Int deriving Show
type Program = [Instruction]
type Memory = IntMap Int

parseInput :: String -> Program
parseInput = map (parseInstruction . words) . lines
    where
        parseInstruction ws
            | isMask ws = Mask (onesMask . last $ ws) (zerosMask . last $ ws) (xsMask . last $ ws)
            | otherwise = Mem (read . takeWhile isDigit . drop 4 . head $ ws) (read . last $ ws)
        onesMask  = sum . map fst . filter ((== '1') . snd) . zip twoPows . reverse
        zerosMask = sum . map fst . filter ((/= '0') . snd) . zip twoPows . reverse
        xsMask    = sum . map fst . filter ((== 'X') . snd) . zip twoPows . reverse
        twoPows   = 1 : map (* 2) twoPows
        isMask    = (== "mask") . head

solvePart1 :: Program -> Int
solvePart1 = foldl1 (+) . fst . foldl execute (M.empty, Mask 0 0 0)
    where
        execute (mem, _)    m@(Mask _ _ _)    = (mem, m)
        execute (mem, mask) (Mem address val) = (M.insertWith const address (maskVal mask val) mem, mask)
        maskVal (Mask one zero _) val         = (val .|. one) .&. zero
        maskVal _ _                           = error "Unknown mask."

solvePart2 :: Program -> Int
solvePart2 = foldl1 (+) . fst . foldl execute (M.empty, Mask 0 0 0)
    where
        execute (mem, _)    m@(Mask _ _ _)    = (mem, m)
        execute (mem, mask) (Mem address val) = (foldl (insert val) mem . enumAdresses mask $ address , mask)
        insert val mem address                = M.insertWith const address val mem
        enumAdresses (Mask os _ xs) address   = enumStep (0 :: Int) xs (address .|. os)
        enumAdresses _              _         = error "Shut up linter."
        enumStep 36 _  a  = [a]
        enumStep i  xs a
            | testBit xs i = (enumStep (succ i) xs (clearBit a i)) ++ (enumStep (succ i) xs (setBit a i))
            | otherwise    = enumStep (succ i) xs a

main :: IO ()
main = do
    is <- parseInput <$> readFile "./input/14_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ is)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ is)