import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Instruction      = Acc Int | Jmp Int | Nop Int deriving Show
data Result           = Terminated Int | Loop Int deriving Show
type Program          = V.Vector Instruction
type InstructionState = U.Vector Bool

parseInput :: String -> Program
parseInput = V.fromList . map parseInstruction . map words . lines . filter (/= '+')
    where
        parseInstruction ["acc", arg] = Acc (read arg)
        parseInstruction ["jmp", arg] = Jmp (read arg)
        parseInstruction ["nop", arg] = Nop (read arg)
        parseInstruction _            = error "Invalid instruction."

run :: Program -> InstructionState -> Int -> Int -> Result
run ps state eip acc | eip == V.length ps = Terminated acc
                     | state U.! eip      = Loop acc
                     | otherwise          = execute (ps V.! eip)
    where
        state'          = state U.// [(eip, True)]
        execute (Acc a) = run ps state' (eip + 1) (acc + a)
        execute (Jmp o) = run ps state' (eip + o) acc
        execute (Nop _) = run ps state' (eip + 1) acc

solvePart1 :: Program -> Result
solvePart1 ps = let state = U.replicate (V.length ps) False
                in run ps state 0 0

solvePart2 :: Program -> Result
solvePart2 ps = let jmpIs = V.toList . V.findIndices isJmp $ ps
                    nopIs = V.toList . V.findIndices isNop $ ps
                in head . dropWhile isLoop . map runModified $ jmpIs ++ nopIs
    where
        state                  = U.replicate (V.length ps) False
        runModified i          = run (toggleInstruction ps i) state 0 0
        toggleInstruction is i = toggle' (is V.! i)
            where
                toggle' (Jmp a) = ps V.// [(i, Nop a)]
                toggle' (Nop a) = ps V.// [(i, Jmp a)]
                toggle' _       = error "Invalid toggle."

        isJmp (Jmp _)   = True
        isJmp _         = False
        isNop (Nop _)   = True
        isNop _         = False
        isLoop (Loop _) = True
        isLoop _        = False

main :: IO ()
main = do
    program <- parseInput <$> readFile "./input/08_day.txt"
    putStrLn $ "Part 1: " ++ (show . solvePart1 $ program)
    putStrLn $ "Part 2: " ++ (show . solvePart2 $ program)