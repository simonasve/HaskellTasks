module Makemove where

import Parser
import ParserMessage
import Data.List
import Data.Char
import System.Exit

size' :: Int 
size' = 3

depth :: Int 
depth = 9

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

data Tree a = Node a [Tree a] deriving Show

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size' (replicate size' B)

full :: Grid -> Bool
full = notElem B . concat

turn :: Grid -> Player
turn g = if xs <= os then X else O
        where
            ps = concat g
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)

wins :: Player -> Grid -> Bool 
wins p g = any line (rows ++ cols ++ dias)
           where
               line = all (== p)
               rows = g
               cols = transpose g
               dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size'-1]]

won :: Grid -> Bool 
won g = wins X g || wins O g

valid :: Grid -> Int -> Bool 
valid g i = 0 <= i && i < size'^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
    [chop size' (xs ++ [p] ++ ys) | valid g i]
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

removeRight :: Either a b -> b
removeRight eith = case eith of
                 Right res -> res

convertToPlayer :: [String] -> Player
convertToPlayer args
              | head (head args) == 'X' = X
              | otherwise = O

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
    | won g     = []
    | full g    = []
    | otherwise = concat [move g i p | i <- [0..((size'^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
    | wins O g = Node (g, O) []
    | wins X g = Node (g, X) []
    | otherwise  = Node (g, B) []
minimax (Node g ts)
    | turn g == O = Node (g, minimum ps) ts'
    | turn g == X = Node (g, maximum ps) ts'
                    where
                        ts' = map minimax ts
                        ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove [[B,B,B],[B,B,B],[B,B,B]] X = [[X,B,B],[B,B,B],[B,B,B]]
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
                where
                    tree = prune depth (gametree g p)
                    Node (_,best) ts = minimax tree

parseLine :: String -> Either String JsonLikeValue
parseLine m =
    case parse size' m of
    Right res -> Right res
    Left em -> Left em

getMoveList :: JsonLikeValue  -> Either InvalidState To
getMoveList jsl =
    case convert size' jsl of
    Right res -> Right res
    Left em -> Left em

convertToGrid :: To -> Int -> Grid -> Grid
convertToGrid [] row g = g
convertToGrid l row g = 
            let
                res = convertToGrid' (head l) []  row
                res' = tranferToGrid res g
            in
                convertToGrid (drop 1 l) (row - 1) res'

convertToGrid' :: [(Int, Char)] -> [(Int, Char)] -> Int -> [(Int,Char)]
convertToGrid' xs acc row = foldr (\x -> (:) (convertToIndex x row)) acc xs

convertToIndex :: (Int, Char) -> Int -> (Int, Char)
convertToIndex (x,c) row
                | row == 0 = (x,c)
                | row == 1 = (x+3, c)
                | row == 2 = (x+6, c)

tranferToGrid :: [(Int, Char)] -> Grid -> Grid
tranferToGrid [] g = g
tranferToGrid l g =
    let
        (i,c) = head l
    in       
        if c == 'X'
        then case move g i X of
             [] -> tranferToGrid (drop 1 l) g
             [g'] -> tranferToGrid (drop 1 l) g'
        else case move g i O of
             [] -> tranferToGrid (drop 1 l) g
             [g'] -> tranferToGrid (drop 1 l) g'

compareGrids :: [Player] -> [Player] -> Int -> Int
compareGrids [] [] count = -1
compareGrids g g' count =
            let
                h1 = head g
                h2 = head g'
            in
                if h1 /= h2
                then count
                else compareGrids (drop 1 g) (drop 1 g') (count - 1)

getCoordinates :: Int -> (Int, Int)
getCoordinates i 
            | i == 0 = (2, 0)
            | i == 1 = (1, 0)
            | i == 2 = (0, 0)
            | i == 3 = (2, 1)
            | i == 4 = (1, 1)
            | i == 5 = (0, 1)
            | i == 6 = (2, 2)
            | i == 7 = (1, 2)
            | i == 8 = (0, 2)

getGameState :: To -> String -> Int -> String
getGameState [] ms row = ms
getGameState (l:ls) ms row = getGameState' l ms row ++ getGameState ls ms (row + 1)

getGameState' :: [(Int, Char)] -> String -> Int -> String
getGameState' [] ms row = ms
getGameState' ((i,c):xs) ms row
            | row == 0 = getGameState' xs (concat [ms, "(", [intToDigit i],",", [intToDigit 0],",",[c],") "]) row
            | row == 1 = getGameState' xs (concat [ms, "(", [intToDigit i],",", [intToDigit 1],",",[c],") "]) row
            | row == 2 = getGameState' xs (concat [ms, "(", [intToDigit i],",", [intToDigit 2],",",[c],") "]) row