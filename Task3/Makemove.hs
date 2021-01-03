module Makemove where

import Parser
import ParserMessage
import CheckIfWon
import Data.List
import Data.Char

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
full = all (/=B) . concat

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
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool 
won g = wins X g || wins O g

putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
          beside = foldr1 (zipWith (++))
          bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = [" ", " O ", " "]
showPlayer B = [" ", " ", " "]
showPlayer X = [" ", " X ", " "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool 
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
    if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                    return (read xs)
                   else
                    do putStrLn "ERROR: Invalid number"
                       getNat prompt

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
    | won g     = []
    | full g    = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

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
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
                where
                    tree = prune depth (gametree g p)
                    Node (_,best) ts = minimax tree

addLetter :: String -> String
addLetter m = m ++ "s"

parseLine :: String -> JsonLikeValue
parseLine m =
    case parse 3 m of
    Right res -> res

getMoveList :: String -> To
getMoveList m = case convert 3 (parseLine m) of
                Right res -> res

makeMoveLogic :: String -> String
makeMoveLogic m = case checkIfWon(getMoveList m) of
                  True -> "won"
                  False -> case (getMoveList m) of
                           [[],[],[]] -> "l4:lastll4:datali0ei0e1:Xeeee"
                           [[x1,y1,v1],[x2,y2,v2],[x3,y3,v3]] -> "tie"
                           _ -> "asd"