module Main where

import Makemove

main :: IO ()
main = getLine >>= (\a -> putStrLn (makeMoveLogic a))
