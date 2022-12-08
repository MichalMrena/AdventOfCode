module Day07 where

import           Data.List ( foldl', sort )
import qualified Data.Map as M
import           Data.Map ( (!) )
import qualified Data.Sequence as S
import           Data.Sequence ( (|>), Seq (Empty, (:<|), (:|>)) )
import qualified Text.Parsec as P
import           Text.Parsec ( (<|>) )

data Tree = Nil
          | File { name_ :: String, size_ :: Int }
          | Dir { name_ :: String, sons_ :: (M.Map String Tree) }
          deriving Show

data Input = Cd String
           | Ls
           | LFile Int String
           | LDir String deriving Show

type Path = S.Seq String

dirSizes :: Tree -> [Int]
dirSizes = map snd . M.toList . go (S.empty) M.empty
  where
    go :: Path -> M.Map Path Int -> Tree -> M.Map Path Int
    go p sizes (File _ _)   = sizes
    go p sizes (Dir n sons) = sizes''
      where sizes'        = foldl' (go (p |> n)) sizes (map snd (M.toList sons))
            sizes''       = M.insert (p |> n) s sizes'
            s             = sum (map fs (map snd (M.toList sons)))
            fs (File _ s) = s
            fs (Dir n' _) = sizes' ! (p |> n |> n')

part1 :: Tree -> Int
part1 = sum . filter (<=100000) . dirSizes

part2 :: Tree -> Int
part2 t = head (dropWhile (<need) sizes)
  where sizes = sort (dirSizes t)
        need  = 30000000 - (70000000 - (last sizes))

solveDay :: IO ()
solveDay = do
  xs <- parseInput <$> readFile "input/07_day.txt"
  putStrLn $ "Part 1: " ++ (show . part1 $ xs)
  putStrLn $ "Part 2: " ++ (show . part2 $ xs)
  where
    parseInput :: String -> Tree
    parseInput = mkTree . map (unpack . (P.parse parseLine "")) . lines
    unpack     = either (error . show) id
    parseLine  = cmd <|> file <|> dir
      where cmd  = P.string "$ " >> (cd <|> ls)
            cd   = Cd    <$> (P.string "cd " *> word)
            ls   = (P.string "ls") >> pure Ls
            file = LFile <$> (nat <* P.char ' ') <*> word
            dir  = LDir  <$> (P.string "dir " *> word)
            word = P.many1 (P.alphaNum <|> P.char '/' <|> P.char '.')
            nat  = read  <$> P.many1 P.digit

    mkTree :: [Input] -> Tree
    mkTree = rmProxy . snd . foldl' go (S.empty, (Dir "root" M.empty))
      where
        go :: (Path, Tree) -> Input -> (Path, Tree)
        go ((path :|> _), tree) (Cd "..") = (path, tree)
        go (path, tree) (Cd n)   = (path |> n, insert path (Dir n M.empty) tree)
        go (path, tree) (Ls)        = (path, tree)
        go (path, tree) (LFile s n) = (path, insert path (File n s) tree)
        go (path, tree) (LDir n)    = (path, insert path (Dir n M.empty) tree)
        rmProxy (Dir _ ss) = ss ! "/"

    insert :: Path -> Tree -> Tree -> Tree
    insert path node tree = go path tree
      where
        go :: Path -> Tree -> Tree
        go S.Empty (Dir n ds) =
          let ds' = M.insertWith (flip const) (name_ node) node ds
          in (Dir n ds')
        go (p :<| ps) (Dir n ds) =
          let ds' = go ps (ds ! p)
          in Dir n (M.adjust (const ds') p ds)