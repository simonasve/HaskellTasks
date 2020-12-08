module Task1 where

import Task1Message
import Data.List as L 
import Data.Char as C

parseInt :: String -> (Int, String)
parseInt s =
        (i1, r2)
        where
            r1 = drop 5 s
            numb = take 1 r1
            i1 = read numb
            r2 = drop 1 r1

parseChar :: String -> (Char, String)
parseChar s =
        (c1, r2)
         where
            r1 = drop 5 s
            c1 = head r1
            r2 = drop 2 r1

parseList :: String -> ((Int, Int, Char), String)
parseList ('l':r) = 
          ((i1,i2,c1),r3)
          where
            (i1,r1) = parseInt r
            (i2,r2) = parseInt r1
            (c1,r3) = parseChar r2

parse :: Int -> String -> From
parse size ('l':r) = parse1 r [] 
parse _ _ = expectedFrom

parse1 :: String -> From -> From
parse1 ('e':t) acc = L.reverse acc
parse1 s acc =
            let
              ((i1,i2,c1),r) = parseList s
            in
               parse1 r ((i1,i2,c1):acc)

convert :: Int -> From -> To
convert size mat = convert1 mat [] [] []

convert1 :: From -> [Int] -> [Int] -> [Char] -> To
convert1 [] i i1 c = (i,i1,c)
convert1 mat i i1 c =
            let
              (i2,i3,c1) = head mat
              mat1 = tail mat
            in
              convert1 mat1 (i ++ [i2]) (i1 ++ [i3]) (c ++ [c1])