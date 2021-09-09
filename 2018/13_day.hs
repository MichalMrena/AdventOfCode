import           Control.Monad.Extra ( iterateM )
import           Control.Monad ( forM_ )
import           Data.Either ( Either(Right) )
import           Data.List ( find, sortBy )
import           Data.Function ( on )
import           Data.Array.Unboxed ( (!), (//) )
import qualified Data.Array.Unboxed as A
import qualified Data.Bifunctor ( second )

data Direction  = West | North | East | South deriving (Show, Eq)
data Turn       = TLeft | Straight | TRight deriving (Show, Eq)
data Cart       = Cart { pos_  :: (Int, Int)
                       , dir_  :: Direction
                       , turn_ :: Turn } deriving Show
type Rails      = A.Array (Int, Int) Char

nextTurn :: Turn -> Turn
nextTurn TLeft    = Straight
nextTurn Straight = TRight
nextTurn TRight   = TLeft

nextDir :: Direction -> Turn -> Direction
nextDir d Straight   = d
nextDir West  TLeft  = South
nextDir West  TRight = North
nextDir North TLeft  = West
nextDir North TRight = East
nextDir East  TLeft  = North
nextDir East  TRight = South
nextDir South TLeft  = East
nextDir South TRight = West

advance :: Cart -> Cart
advance (Cart (r, c) West t)  = Cart (r, c - 1) West t
advance (Cart (r, c) East t)  = Cart (r, c + 1) East t
advance (Cart (r, c) North t) = Cart (r - 1, c) North t
advance (Cart (r, c) South t) = Cart (r + 1, c) South t

turn :: Cart -> Cart
turn (Cart p d t) = Cart p (nextDir d t) (nextTurn t)

bend :: Char -> Cart -> Cart
bend '/'  (Cart p North t) = Cart p East t
bend '/'  (Cart p West t)  = Cart p South t
bend '/'  (Cart p South t) = Cart p West t
bend '/'  (Cart p East t)  = Cart p North t
bend '\\' (Cart p North t) = Cart p West t
bend '\\' (Cart p West t)  = Cart p North t
bend '\\' (Cart p South t) = Cart p East t
bend '\\' (Cart p East t)  = Cart p South t
bend _    _                = undefined

move :: Char -> Cart -> Cart
move '+'   = turn
move '/'   = advance . bend '/'
move '\\'  = advance . bend '\\'
move _     = advance

part1 :: Rails -> [Cart] -> (Int, Int)
part1 rails initCs = case iterateM moveCarts initCs of
                         (Left (y, x)) -> (x, y)
                         (Right _) -> undefined
    where
        moveCarts :: [Cart] -> Either (Int, Int) [Cart]
        moveCarts = go [] . sortBy (compare `on` pos_)
            where
                go ls [] = Right ls
                go ls (r:rs) = case find (\x -> pos_ x == pos_ r') ls of
                                   Nothing -> go (r' : ls) rs
                                   (Just _) -> Left (pos_ r')
                    where
                        r' = move (rails ! pos_ r) r

main :: IO ()
main = do
    ((nrow, ncol), railsCs) <- readToArr <$> readFile "./input/13_day.txt"
    let cartPs   = filter ((`elem` ['<', '>', 'v', '^']) . snd)
                 . A.assocs $ railsCs
    let rails    = railsCs // map (Data.Bifunctor.second arrToBar) cartPs
    let carts    = map (\(p, c) -> Cart p (arrToDir c) TLeft) cartPs

    putStrLn $ "Part 1: " ++ show (part1 rails carts)

    -- putStrLn $ show nrow ++ " x " ++ show ncol
    -- forM_ [0 .. nrow - 1] (\row -> do
    --     forM_ [0 .. ncol - 1] (\col ->
    --         putChar (railsCs ! (row, col)))
    --     putStrLn "")

    -- print cartPs

    where
        readToArr :: String -> ((Int, Int), Rails)
        readToArr str = ((nrow, ncol), arr)
            where
                ls   = lines str
                nrow = length ls
                ncol = length . head $ ls
                arr  = A.listArray ((0, 0), (nrow - 1, ncol - 1)) . concat $ ls

        arrToBar :: Char -> Char
        arrToBar '<' = '-'
        arrToBar '>' = '-'
        arrToBar 'v' = '|'
        arrToBar '^' = '|'
        arrToBar _   = undefined

        arrToDir '<' = West
        arrToDir '>' = East
        arrToDir 'v' = South
        arrToDir '^' = North
        arrToDir _   = undefined


-- testm :: IO ()
-- testm = do

--     case iterateM' go [] of
--         (Right _) -> putStrLn "not good"
--         (Left s) -> putStrLn s

--     where
--         go :: [Int] -> Either String [Int]
--         go [] = Right [1]
--         go ys@(x:xs) | x < 10 = Right ((x+1):ys)
--                      | otherwise = Left "Done."

-- iterateM' :: (a -> Either String a) -> a -> Either String [a]
-- iterateM' f x = do
--     x' <- f x
--     (x':) `fmap` iterateM' f x'
--     -- f x >>= (\x' ->
--     --    (x':) `fmap` iterateM' f x' )