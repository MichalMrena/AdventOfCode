import Data.List (maximumBy, foldl')
import Data.Function (on)
import Data.Maybe (fromJust)
import qualified Data.IntMap as M

data CircularList a = CircularList { pre   :: [a]
                                   , focus :: a
                                   , post  :: [a] }

instance (Show a) => Show (CircularList a) where
    show (CircularList ls f rs) = show (reverse ls) ++ " " ++ show f ++ " " ++ show rs

justOne :: a -> CircularList a
justOne x = CircularList [] x []

moveRight :: CircularList a -> CircularList a
moveRight (CircularList [] f [])       = CircularList [] f []
moveRight (CircularList ls f [])       = let (l : ls') = reverse ls
                                         in CircularList [f] l ls'
moveRight (CircularList ls f (r : rs)) = CircularList (f : ls) r rs

moveLeft :: CircularList a -> CircularList a
moveLeft (CircularList [] f [])       = CircularList [] f []
moveLeft (CircularList [] f rs)       = let (r : rs') = reverse rs
                                        in CircularList rs' r [f]
moveLeft (CircularList (l : ls) f rs) = CircularList ls l (f : rs)

moveBy :: Int -> CircularList a -> CircularList a
moveBy 0 xs = xs
moveBy n xs | n < 0 = moveBy (n + 1) (moveLeft xs)
            | n > 0 = moveBy (n - 1) (moveRight xs)

deleteRight :: CircularList a -> CircularList a
deleteRight (CircularList ls _ [])       = let (l : ls') = reverse ls
                                           in CircularList [] l ls'
deleteRight (CircularList ls _ (x : xs)) = CircularList ls x xs

insertLeft :: a -> CircularList a -> CircularList a
insertLeft x (CircularList ls f rs) = CircularList ls x (f : rs)

play :: Int -> Int -> Int
play playerCount marbleCount = maximum $ map snd $ M.toList results
    where
        players = map (`mod` playerCount) [0, 1 ..]
        marbles = [1 .. marbleCount]
        steps   = zip players marbles
        results = fst $ foldl' step (M.empty , justOne 0) steps

        step (score, circle) (p, m) = (score', circle')
            where
                (s, circle') = emplace circle m
                score'       = M.insertWith (+) p s score

        emplace circle m
            | m `mod` 23 == 0 = let circle' = moveBy (-7) circle
                                    m'      = focus circle'
                                in (m + m', deleteRight circle')

            | otherwise       = let circle' = insertLeft m $ moveBy 2 circle
                                in (0, circle')

main :: IO ()
main = do
    (playerCount, marbleCount) <- parseInput <$> readFile "./input/09_day.txt"
    putStrLn $ "Part 1: " ++ show (play playerCount marbleCount)
    putStrLn $ "Part 2: " ++ show (play playerCount (marbleCount * 100))
    where
        parseInput s = (read $ head ws, read $ ws !! 6) where ws = words s