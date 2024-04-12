{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Parameters
-}

module Parameters (
    getPeriod
) where

import System.Exit (exitWith, ExitCode(..))
import Text.Read(readMaybe)
import System.Environment (getArgs)

parsePeriod :: Maybe Int -> IO Int
parsePeriod Nothing = exitWith $ ExitFailure 84
parsePeriod (Just p) = return p

parseArgs :: [String] -> IO Int
parseArgs [s] = parsePeriod $ readMaybe s
parseArgs _ = exitWith $ ExitFailure 84

getPeriod :: IO Int
getPeriod = do
    args <- getArgs
    parseArgs args
