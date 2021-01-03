module Main where
import Makemove
import System.IO

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty X

play :: Grid -> Player -> IO ()
play g p = do putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
    | wins O g = putStrLn "Player O wins!\n"
    | wins X g = putStrLn "Player X wins!\n"
    | full g = putStrLn "It’s a draw!\n"
    | p == X = do i <- getNat (prompt p)
                  case move g i p of
                    [] -> do putStrLn "ERROR: Invalid move"
                             play' g p
                    [g'] -> play g' (next p)
    | p == O = do putStr "Player X is thinking... "
                  (play $! bestmove g p) (next p)

      
