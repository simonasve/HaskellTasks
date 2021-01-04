module Parser where

import ParserMessage
import Data.List as L ( concat, takeWhile, sort, reverse ) 
import Data.Char as C

parseJLInt :: String -> Either (String, Int) (JsonLikeValue, String, Int)
parseJLInt ('i':t) =
    let
        prefix = L.takeWhile C.isDigit t
        postfix = drop (length prefix) t
        size = length prefix + 2
    in
        case postfix of
            ('e':r) -> if prefix == "" then Left ("Invalid integer", size) 
                                              else Right (JLInt (read prefix), r, size)
            _ -> Left ("Invalid integer", size)

parseJLString :: String -> Either String (JsonLikeValue, String, Int)
parseJLString t =
        case C.isDigit (head t) of
        True -> Right (JLString text, restOfText, size)
                where
                prefix = L.takeWhile C.isDigit t
                postfix = drop (length prefix) t
                number = read prefix
                dropColon = drop 1 postfix
                text = take number dropColon
                restOfText = drop number dropColon
                size = length prefix + number + 1
        False -> Left "Invalid String"

parseJLArray :: String -> [JsonLikeValue] -> Int -> Either (String, Int) (JsonLikeValue, String, Int)
parseJLArray m acc size =
    case length acc of
    0 -> case head m of
         'i' -> case parseJLInt m of
                  Right (result, restOfText, size1) -> parseJLArray restOfText (result : acc) (size + size1)
                  Left (em, size1) -> Left (em, size + size1)
         'l' -> case m !! 1 of
                  'l' -> case parseJLArray (drop 1 m) [] 1 of
                           Right (result, restOfText, size1) -> parseJLArray restOfText (result : acc) (size + size1)
                           Left (em, size1) -> Left (em, size + size1)
                  _ -> parseJLArray (drop 1 m) acc (size + 1)
         _ -> if C.isDigit (head m) then
            case parseJLString m of
            Right (result, restOfText, size1) -> parseJLArray restOfText (result : acc) (size + size1)
            Left em -> Left (em, size)
            else Left ("Invalid array", size)
    _ -> case head m of
        'i' -> case parseJLInt m of
                Right (result, restOfText, size1) -> parseJLArray restOfText (result : acc) (size + size1)
                Left (em, size1) -> Left (em, size + size1)
        'l' -> case parseJLArray m [] 0 of
                Right (result, restOfText, size1) -> parseJLArray restOfText (result : acc) (size + size1)
                Left (em, size1) -> Left (em, size + size1)
        'e' -> Right (JLArray (L.reverse acc), drop 1 m, size + 1)
        _ -> if C.isDigit (head m) then
            case parseJLString m of
            Right (result, restOfText, size1) -> parseJLArray restOfText (result : acc) (size + size1)
            Left em -> Left (em, size)
            else Left ("Invalid array", size)

parse :: Int -> String -> Either String JsonLikeValue
parse size m =
    if m /= "*"
    then
        case parseJLArray m [] 0 of
        Right (result, restOfText, size1) -> Right result
        Left (em, size2) -> Left $ L.concat ["Error in position ", show (size2 + 1), ": ", em]
    else Right (JLArray [])

convert :: Int -> JsonLikeValue -> Either InvalidState To
convert size (JLArray v) =
    case convert' (getMoves v []) [] of
        Right res -> case checkForOrder res of
                     Right True -> case convertToLIL res res [] [] (size - 1) of
                                   Right res -> Right res
                                   Left Duplicates -> Left Duplicates
                     Left Order -> Left Order
        Left em -> Left em

convert' :: [[JsonLikeValue]] -> [(Int, Int ,Char)] -> Either InvalidState [(Int, Int ,Char)]
convert' [] [] = Right []
convert' [] acc = Right acc
convert' arr acc =
    case extractMove (head arr) of
    Right result -> convert' (drop 1 arr) (result:acc)
    Left em -> Left em

extractMove :: [JsonLikeValue] -> Either InvalidState (Int, Int, Char)
extractMove [JLInt x,JLInt y,JLString v] = Right (x,y,head v)

getMoves :: [JsonLikeValue] -> [[JsonLikeValue]] -> [[JsonLikeValue]]
getMoves [] [] = []
getMoves ((JLString "last"):JLArray [JLArray [JLString "data", JLArray m]]:(JLString "prev"):t) acc =
    case head t of
    JLArray a -> getMoves a (m:acc)
getMoves ((JLString "prev"):JLArray m:(JLString "last"):JLArray [JLArray [JLString "data", JLArray a]]:t) acc = getMoves m (a:acc)
getMoves [JLString "last",JLArray [JLArray [JLString "data", JLArray m]]] acc = m:acc

checkForOrder :: [(Int, Int ,Char)] -> Either InvalidState Bool
checkForOrder [] = Right True
checkForOrder mat =
    case length (drop 1 mat) of
    0 -> checkForOrder (drop 1 mat)
    _ -> let
            (i1,i2,c1) = head mat
            restOfList = drop 1 mat
            (i3,i4,c2) = head restOfList
         in
            if c1 == c2
            then Left Order
            else checkForOrder restOfList

convertToLIL :: [(Int, Int, Char)] -> [(Int, Int, Char)] -> [(Int, Char)] -> To-> Int -> Either InvalidState To
convertToLIL [] [] [] [] size = Right [[],[],[]]
convertToLIL [] mat tempAcc acc size = 
    if (size - 1) /= -2
    then convertToLIL mat mat [] (sort tempAcc:acc) (size - 1)
    else case checkForDuplicate acc of
         Right True -> Right acc
         Left Duplicates -> Left Duplicates
convertToLIL mat mat1 tempAcc acc size =
    let
        (i1,i2,c) = head mat
        restOfList = drop 1 mat
    in
        if i2 == size
        then convertToLIL restOfList mat1 ((i1,c):tempAcc) acc size
        else convertToLIL restOfList mat1 tempAcc acc size

checkForDuplicate :: To -> Either InvalidState Bool
checkForDuplicate [] = Right True
checkForDuplicate mat1 =
    let
        fList = head mat1
        restOfList = drop 1 mat1
    in
        case checkForDuplicate' fList of
        Right True -> checkForDuplicate restOfList
        Left em -> Left em

checkForDuplicate' :: [(Int,Char)] -> Either InvalidState Bool
checkForDuplicate' [] = Right True
checkForDuplicate' mat =
    case length (drop 1 mat) of
    0 -> checkForDuplicate' (drop 1 mat)
    _ -> let
            (i1,c1) = head mat
            restOfList = drop 1 mat
            (i2,c2) = head restOfList
        in
            if i1 == i2
            then Left Duplicates
            else checkForDuplicate' restOfList
