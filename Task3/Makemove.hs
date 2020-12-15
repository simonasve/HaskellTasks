module Makemove where

import Parser
import ParserMessage
import CheckIfWon

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