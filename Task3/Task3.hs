module Main where
import Makemove
import System.IO
import System.Exit
import System.Environment
import Data.Char
import Control.Monad ( when )

main :: IO () 
main = do
        args <- getArgs
        m <- getLine
        let jsonValue = parseLine m
        case jsonValue of
          Left em -> do 
                      putStrLn em 
                      exitWith (ExitFailure 100)
          Right _ -> return ()
        let moveList = getMoveList $ removeRight jsonValue
        case moveList of
          Left _ -> do
                      putStrLn "Incoming message is semanticallly invalid"
                      exitWith (ExitFailure 101)
          Right _ -> return ()
        let grid = convertToGrid (removeRight moveList) (size' - 1) empty
        Control.Monad.when (full grid || won grid) $ do 
          putStrLn "I cannot perform any moves because game is already ended"
          exitWith (ExitFailure 20)
        let grid' = bestmove grid (convertToPlayer args)
        let index = compareGrids (concat grid) (concat grid') 8
        let (x,y) = getCoordinates index
        let c = head (head args)
        let stdrMessage = concat ["Game state: ", getGameState (removeRight moveList) "" 0, ". My move: (",[intToDigit x],",", [intToDigit y], ",",[c],")"]
        if wins (convertToPlayer args) grid'
          then do
                putStrLn (concat ["l4:lastll4:datali",[intToDigit x],"ei",[intToDigit y],"e1:",[c],"eee4:prev",m,"e"])
                hPutStrLn stderr (stdrMessage ++ ".I win")
                exitWith (ExitFailure 10)
          else if full grid'
                then do 
                      putStrLn (concat ["l4:lastll4:datali",[intToDigit x],"ei",[intToDigit y],"e1:",[c],"eee4:prev",m,"e"])
                      hPutStrLn stderr (stdrMessage ++ ". A draw")
                      exitWith (ExitFailure 12)
                else if m /= "*"
                  then do
                        putStrLn (concat ["l4:lastll4:datali",[intToDigit x],"ei",[intToDigit y],"e1:",[c],"eee4:prev",m,"e"])
                        hPutStrLn stderr stdrMessage
                  else do
                        putStrLn (concat ["l4:lastll4:datali",[intToDigit x],"ei",[intToDigit y],"e1:",[c],"eeee"])
                        hPutStrLn stderr stdrMessage