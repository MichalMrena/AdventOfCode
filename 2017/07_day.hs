import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    xs <- parse <$> readFile "./input/06_day.txt"
    putStrLn "done"

    where
        parseLine m s = M.empty
        parse = foldl parseLine M.empty . lines