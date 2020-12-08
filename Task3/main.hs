module Main where

import Makemove ( addLetter )

main :: IO ()
main = getLine >>= (\a -> putStrLn (addLetter a))
