--
-- EPITECH PROJECT, 2024
-- Groundhog
-- File description:
-- Spec
--

import Test.HUnit
import Parameters (parseArgs)
import Control.Exception (catch)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
    tests <- parseArgsTests
    runTestTTAndExit tests

parseArgsTakesOnePosInt :: IO Test
parseArgsTakesOnePosInt = do
    result <- parseArgs ["2"]
    return $ TestCase $ assertEqual "parseArgs 2" 2 result

parseArgsWithMany :: IO Test
parseArgsWithMany = do
    result <- catch (parseArgs ["2", "3"]) handler
    return $ TestCase $ assertEqual "parseArgs 2 3 exit" 84 result
    where
        handler :: ExitCode -> IO Int
        handler (ExitFailure n) = return n
        handler _ = return 0

parseArgsWithNone :: IO Test
parseArgsWithNone = do
    result <- catch (parseArgs []) handler
    return $ TestCase $ assertEqual "parseArgs [] exit" 84 result
    where
        handler :: ExitCode -> IO Int
        handler (ExitFailure n) = return n
        handler _ = return 0

parseArgsBadValue :: IO Test
parseArgsBadValue = do
    result <- catch (parseArgs ["Foo"]) handler
    return $ TestCase $ assertEqual "parseArgs Foo exit" 84 result
    where
        handler :: ExitCode -> IO Int
        handler (ExitFailure n) = return n
        handler _ = return 0

parseArgsTests :: IO Test
parseArgsTests = do
    onePosInt <- parseArgsTakesOnePosInt
    withMany <- parseArgsWithMany
    none <- parseArgsWithNone
    badValue <- parseArgsBadValue
    return $ TestList [
        TestLabel "One positive int" onePosInt,
        TestLabel "With many args" withMany,
        TestLabel "With no arg" none,
        TestLabel "A string" badValue
        ]