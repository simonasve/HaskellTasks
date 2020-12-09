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

getMoveList :: JsonLikeValue -> To
getMoveList m = case convert 3 m of
                Right res -> res

makeMoveLogic :: String -> String
makeMoveLogic m = if checkIfWon(getMoveList (parseLine m)) then "True" else "False"