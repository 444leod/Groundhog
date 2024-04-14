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

{- | The `parsePeriod` function tries to parse a **natural** `Int`
-- from a `String`.
--
-- It takes a `String` to render error messages. (Usually the phrase parsed.)
-- It takes a `Maybe` value - the result of a `readMaybe`.
--
-- >>> parsePeriod "Hello" (readMaybe "Hello" :: Maybe Int)
-- Given argument is not a natural number: Hello
-- *** Exception: ExitFailure 84
--
-- >>> parsePeriod "12" (readMaybe "12" :: Maybe Int)
-- 12
--
-}
parsePeriod :: String -> Maybe Int -> IO Int
parsePeriod s (Just p)
    | p > 0 = return p
    | otherwise =
        (hPutStrLn stderr $ "Cannot accept negative or null value " ++ s) >>
        exitWith (ExitFailure 84)
parsePeriod s Nothing =
    hPutStrLn stderr ("Given argument is not a natural number: " ++ s) >>
    exitWith (ExitFailure 84)

{- | The `parseArgs` function is the core of the argument parsing.
-- It takes an `Array` of `String` (`[String]`) (the parameters).
--
-- Its goal is to get a single int as argument.
-- If the conditions are unmet, an error message will be printed
-- followed by an`ExitFailure`.
--
-- >>> parseArgs ["12"]
-- 12
--
-- >>> parseArgs []
-- Wrong argument count. 0 arguments given. 1 awaited.
-- *** Exception: ExitFailure 84
--
-- >>> parseArgs ["Hello"]
-- Given argument is not a natural number: Hello
-- *** Exception: ExitFailure 84
--
-}
parseArgs :: [String] -> IO Int
parseArgs [s] = parsePeriod s $ readMaybe s
parseArgs xs =
    hPutStrLn stderr ("Wrong argument count. "
        ++ (show $ length xs) ++ " arguments given. 1 awaited.") >>
    exitWith (ExitFailure 84)

{-
-- | The `getPeriod` function is the root of the argument parsing.
--
-- It uses `getArgs` to retreive the sent parameters.
-- Then uses `parseArgs` on these values. (see documentation)
-- It returns the result as an `IO` value.
-}
getPeriod :: IO Int
getPeriod = do
    args <- getArgs
    parseArgs args
