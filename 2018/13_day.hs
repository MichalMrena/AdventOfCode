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
move '+'   = advance . turn
move '/'   = advance . bend '/'
move '\\'  = advance . bend '\\'
move _     = advance

part1 :: Rails -> [Cart] -> (Int, Int)
part1 rails initCs = case iterateM moveCarts initCs of
                         (Left (y, x)) -> (x, y)
                         (Right _)     -> undefined
    where
        moveCarts :: [Cart] -> Either (Int, Int) [Cart]
        moveCarts = go [] . sortBy (compare `on` pos_)
            where
                go ls [] = Right ls
                go ls (r:rs) = case find ((== pos_ r'). pos_) (ls ++ rs) of
                                   Nothing  -> go (r' : ls) rs
                                   (Just _) -> Left (pos_ r')
                    where
                        r' = move (rails ! pos_ r) r

part2 :: Rails -> [Cart] -> (Int, Int)
part2 rails initCs = case iterateM moveCarts initCs of
                         (Left (y, x)) -> (x, y)
                         (Right _)     -> undefined
    where
        moveCarts :: [Cart] -> Either (Int, Int) [Cart]
        moveCarts cs = case go [] . sortBy (compare `on` pos_) $ cs of
                           [c] -> Left (pos_ c)
                           cs' -> Right cs'
            where
                go ls []     = ls
                go ls (r:rs) = case find ((== pos_ r') . pos_) (ls ++ rs) of
                                   Nothing  -> go (r' : ls) rs
                                   (Just _) -> go ls' rs'
                    where
                        r'  = move (rails ! pos_ r) r
                        ls' = filter ((/= pos_ r') . pos_) ls
                        rs' = filter ((/= pos_ r') . pos_) rs

main :: IO ()
main = do
    ((nrow, ncol), railsCs) <- readToArr <$> readFile "./input/13_day.txt"
    let cartPs   = filter ((`elem` ['<', '>', 'v', '^']) . snd)
                 . A.assocs $ railsCs
    let rails    = railsCs // map (Data.Bifunctor.second arrToBar) cartPs
    let carts    = map (\(p, c) -> Cart p (arrToDir c) TLeft) cartPs

    putStrLn $ "Part 1: " ++ show (part1 rails carts)
    putStrLn $ "Part 2: " ++ show (part2 rails carts)

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