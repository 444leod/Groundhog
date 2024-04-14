{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Input
-}

module Input (
    getInput,
    parseInput
) where

import System.IO (hPutStrLn, stderr)
import Control.Exception(catch, IOException)
import System.Exit (exitWith, ExitCode(..))
import Text.Read (readMaybe)

{- | The `getDouble` function is used to throw if the given user input
-- was invalid.
--
-- It takes a `Maybe` value (parsed from a `readMaybe` for example).
-- And throws if it finds `Nothing`. Or returns the `Just` value.
--
-- >>> getDouble (readMaybe "Hello" :: Maybe Double)
-- Bad input detected. Stoping as failure.
-- *** Exception: ExitFailure 84
--
-- >>> getDouble (readMaybe "12.56" :: Maybe Double)
-- 12.56
--
-}
getDouble :: Maybe Double -> IO Double
getDouble Nothing =
    hPutStrLn stderr "Bad input detected. Stoping as failure." >>
    exitWith (ExitFailure 84)
getDouble (Just f) = return f

{- | The `parseInput` function takes a `String` and tries to parse a `Double`
-- from the given string.
--
-- If the given string was `STOP`, it will exit the program gracefully.
-- Otherwise it will parse using `getDouble` (see documentation).
--
-- >>> parseInput "STOP"
-- *** Exception: ExitSuccess
--
-- >>> parseInput "Hello"
-- Bad input detected. Stoping as failure.
-- *** Exception: ExitFailure 84
--
-- >>> parseInput "12.56"
-- 12.56
--
-}
parseInput :: String -> IO Double
parseInput "STOP" = exitWith ExitSuccess
parseInput s = getDouble (readMaybe s :: Maybe Double)

{- | A basic handler used to catch unexpected EOF -}
handleExit :: IOException -> IO String
handleExit _ =
    hPutStrLn stderr "EOF Detected. Stoping as failure." >>
    hPutStrLn stderr "Please use keyword 'STOP' to exit successfully." >>
    exitWith (ExitFailure 84)

{- | The `getInput` function creates a blocking call to read the
-- **standard input**.
-- Once a **line** has been read, it uses `parseInput` to retrieve a `Double`.
-- It catches EOFs using a simple handler.
-}
getInput :: IO Double
getInput = do
    s <- catch getLine handleExit
    parseInput s
