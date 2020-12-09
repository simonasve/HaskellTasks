module CheckIfWon where

import ParserMessage

checkIfWon :: To -> Bool
checkIfWon m = case checkRow m of
               True -> True
               False -> case checkColumn m of
                        True -> True
                        False -> case checkDiagonal1 m of
                                 True -> True
                                 False -> case checkDiagonal1 m of
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

checkColumn :: To -> Bool
checkColumn ([]:t:m) = False
checkColumn m =
    let
        t = head (head m)
        rest1 = m !! 1
        rest2 = m !! 2
        rest3 = drop 1 (m !! 0)
    in
        case checkColumn' rest1 t of
        True -> case checkColumn' rest2 t of
                True -> True
                False -> checkColumn [rest3,rest1,rest2]
        False -> False

checkColumn' :: [(Int, Char)] -> (Int, Char) -> Bool
checkColumn' [] t = False
checkColumn' m (i1,c1) =
        let
            (i2,c2) = head m
            rest = drop 1 m
        in
            if (i1 == i2 && c1 == c2)
            then True
            else checkColumn' rest (i1,c1)

checkDiagonal1 :: To -> Bool
checkDiagonal1 ([]:t:m) = False
checkDiagonal1 (t:[]:m) = False
checkDiagonal1 (t:m:[]) = False
checkDiagonal1 m = 
    case checkDiagonal1' (m !! 0) 0 'a' of
    (True, c) -> case checkDiagonal1' (m !! 1) 1 c of
                 (True, c) -> case checkDiagonal1' (m !! 2) 2 c of
                              (True, c) -> True
                              (False, c) -> False
                 (False, c) -> False 
    (False, c) -> False

checkDiagonal1' :: [(Int, Char)] -> Int -> Char -> (Bool,Char)
checkDiagonal1' [] n c = (False, c)
checkDiagonal1' m n c =
        let
            (i1, c1) = head m
        in
            if (n == 0 && i1 == 0)
            then (True, c1)
            else if (i1 == n && c1 == c)
                 then (True, c)
                 else checkDiagonal1' (drop 1 m) n c

checkDiagonal2 :: To -> Bool
checkDiagonal2 (t:m:[]) = False
checkDiagonal2 (t:[]:m) = False
checkDiagonal2 ([]:t:m) = False
checkDiagonal2 m = 
    case checkDiagonal2' (m !! 0) 2 'a' of
    (True, c) -> case checkDiagonal2' (m !! 1) 1 c of
                 (True, c) -> case checkDiagonal2' (m !! 2) 0 c of
                              (True, c) -> True
                              (False, c) -> False
                 (False, c) -> False 
    (False, c) -> False

checkDiagonal2' :: [(Int, Char)] -> Int -> Char -> (Bool,Char)
checkDiagonal2' [] n c = (False, c)
checkDiagonal2' m n c =
        let
            (i1, c1) = head m
        in
            if (n == 2 && i1 == 2)
            then (True, c1)
            else if (i1 == n && c1 == c)
                 then (True, c)
                 else checkDiagonal2' (drop 1 m) n c