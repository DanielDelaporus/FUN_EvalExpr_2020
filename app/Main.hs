module Main where

import System.Environment
import System.Exit
import Lib

main :: IO ()
main = do
    args <- getArgs
    case ((length args) /= 1) of
        True -> do
            putStrLn "invalid number of args"
            exitWith (ExitFailure 84)
        False -> do
            return()

    case evalExpr 600 (addParr ((filter (/=' ') (args !! 0)))) of
        Just a -> do
            putStrLn (show (a))
            return ()
        Nothing -> do
            putStrLn "Error"
            exitWith (ExitFailure 84)
    exitWith (ExitSuccess)
