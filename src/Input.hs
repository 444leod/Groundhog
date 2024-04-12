{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Input
-}

module Input (
    getInput
) where

import System.IO (hPutStrLn, stderr)
import Control.Exception(catch, IOException)
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)

getDouble :: Maybe Double -> IO Double
getDouble Nothing = exitWith (ExitFailure 84)
getDouble (Just f) = return f

parseInput :: String -> IO Double
parseInput "STOP" = exitWith ExitSuccess
parseInput s = getDouble (readMaybe s :: Maybe Double)

handleExit :: IOException -> IO String
handleExit _ =
    hPutStrLn stderr "EOF Detected. Stoping as failure." >>
    hPutStrLn stderr "Please use keyword 'STOP' to exit successfully." >>
    exitWith (ExitFailure 84)

getInput :: IO Double
getInput = do
    s <- catch getLine handleExit
    parseInput s
