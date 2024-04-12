{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Parameters
-}

module Parameters (
    getPeriod,
    parseArgs
) where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import Text.Read(readMaybe)
import System.Environment (getArgs)

parsePeriod :: String -> Maybe Int -> IO Int
parsePeriod s (Just p)
    | p > 0 = return p
    | otherwise = do
        hPutStrLn stderr $ "Cannot accept negative or null value " ++ s
        exitWith $ ExitFailure 84
parsePeriod s Nothing = do
    hPutStrLn stderr $ "Given argument is not a natural number: " ++ s
    exitWith $ ExitFailure 84

parseArgs :: [String] -> IO Int
parseArgs [s] = parsePeriod s $ readMaybe s
parseArgs xs = do
    hPutStrLn stderr $ "Wrong argument count. "
        ++ (show $ length xs) ++ " arguments given. 1 awaited."
    exitWith $ ExitFailure 84

getPeriod :: IO Int
getPeriod = do
    args <- getArgs
    parseArgs args
