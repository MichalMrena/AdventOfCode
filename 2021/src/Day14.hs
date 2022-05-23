module Day14 where

import           Data.List ( foldl' )
import           Data.Map ( Map, (!) )
import qualified Data.Map as M
import qualified Text.Parsec as P

solve :: Int -> (String, Map String Char) -> Integer
solve i (initPolymer, rules) = maximum counts - minimum counts
  where initPs        = zipWith (\a b -> [a, b]) initPolymer (tail initPolymer)
        initFreq      = M.fromListWith (+) (zip initPs (repeat 1))
        emptyFreq     = M.fromList ((map fst . M.toList $ rules) `zip` repeat 0)
        initPairFreq  = M.unionWith (+) emptyFreq initFreq
        finalPairFreq = iterate go initPairFreq !! i
        charFreq      = M.mapKeysWith (+) head finalPairFreq
        charFreq'     = M.adjust succ (last initPolymer) charFreq
        counts        = map snd . M.toList $ charFreq'

        go :: Map String Integer -> Map String Integer
        go m = m''
          where m'  = fmap (const 0) m
                ps  = map fst . filter ((>0) . snd) $ M.toList m
                m'' = foldl' grow m' ps

                grow acc [a,b] = M.adjust (+ c) p2 $ M.adjust (+ c) p1 acc
                  where p1 = [a, rules ! [a,b]]
                        p2 = [rules ! [a,b], b]
                        c  = m ! [a,b]

part1 :: (String, Map String Char) -> Integer
part1 = solve 10

part2 :: (String, Map String Char) -> Integer
part2 = solve 40

solveDay :: IO ()
solveDay = do
  input <- parseInput <$> readFile "input/14_day.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
  where parseInput str = (polymer, M.fromList rules)
          where ls = lines str
                polymer  = head ls
                rulesStr = drop 2 ls
                rules    = map (unpack . P.parse rule "") rulesStr
        unpack = either (error . show) id
        rule   = do key <- P.count 2 P.letter
                    P.string " -> "
                    val <- P.letter
                    return (key, val)