module Makemove where

import Parser
import ParserMessage

addLetter :: String -> String
addLetter m = m ++ "s"

parseLine :: String -> JsonLikeValue
parseLine m =
    case parse 3 m of
    Right res -> res

getMoveList :: JsonLikeValue -> To
getMoveList m = case convert 3 m of
                Right res -> res

makeMoveLogic :: String -> To
makeMoveLogic m = getMoveList (parseLine m)

checkIfWon :: To -> Bool
checkIfWon m = case checkRow m of
               True -> True
               False -> False

checkRow :: To -> Bool
checkRow m 
    |(checkRow' (head m) == True) = True
    |(checkRow' (head (drop 1 m)) == True) = True
    |(checkRow' (head (drop 2 m)) == True) = True
    |otherwise = False

checkRow' ::[(Int, Char)] -> Bool
checkRow' m = case (length m == 3) of
    True -> let
            (i1,c1) = head m
            rest1 = drop 1 m
            (i2,c2) = head rest1
            rest2 = drop 2 m
            (i3,c3) = head rest2
            in
                if(c1==c2 && c2==c3)
                then True
                else False
    False -> False