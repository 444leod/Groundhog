--
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Input
--

module Input (
    getInput
) where


import Control.Exception(catch, IOException)
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)

getDouble :: Maybe Double -> IO Double
getDouble Nothing = exitWith (ExitFailure 84)
getDouble (Just f) = return f

parseInput :: String -> IO Double
parseInput "STOP" = exitWith ExitSuccess
parseInput s = getDouble (readMaybe s :: Maybe Double)

getInput :: IO Double
getInput = do
    s <- catch (getLine) ((\_ -> exitWith (ExitFailure 84)) :: IOException -> IO String)
    parseInput s
