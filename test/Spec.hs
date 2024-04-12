--
-- EPITECH PROJECT, 2024
-- Groundhog
-- File description:
-- Spec
--

import Test.HUnit
import Parameters (parseArgs)
import Input (parseInput)
import Control.Exception (catch)
import System.Exit (ExitCode(..))

main :: IO ()
main = do
    tests <- testSuites
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

parseArgsWithString :: IO Test
parseArgsWithString = do
    result <- catch (parseArgs ["Foo"]) handler
    return $ TestCase $ assertEqual "parseArgs Foo exit" 84 result
    where
        handler :: ExitCode -> IO Int
        handler (ExitFailure n) = return n
        handler _ = return 0

parseArgsNegativeInt :: IO Test
parseArgsNegativeInt = do
    result <- catch (parseArgs ["-1"]) handler
    return $ TestCase $ assertEqual "parseArgs -1 exit" 84 result
    where
        handler :: ExitCode -> IO Int
        handler (ExitFailure n) = return n
        handler _ = return 0

parseInputTakesOneDouble :: IO Test
parseInputTakesOneDouble = do
    result <- parseInput "12.34"
    return $ TestCase $ assertEqual "parseInput 12.34" 12.34 result

parseInputOneDoublePadding :: IO Test
parseInputOneDoublePadding = do
    result <- parseInput "   \t \t 12.34  \t "
    return $ TestCase $ assertEqual "parseInput [padding] 12.34" 12.34 result

parseInputEmpty :: IO Test
parseInputEmpty = do
    result <- catch (parseInput "") handler
    return $ TestCase $ assertEqual "parseInput \"\"" 0 result
    where
        handler :: ExitCode -> IO Double
        handler (ExitFailure 84) = return 0
        handler _ = return $ -1

parseInputMany :: IO Test
parseInputMany = do
    result <- catch (parseInput "12.34 56.78") handler
    return $ TestCase $ assertEqual "parseInput 12.34 56.78" 0 result
    where
        handler :: ExitCode -> IO Double
        handler (ExitFailure 84) = return 0
        handler _ = return $ -1

parseArgsTests :: IO Test
parseArgsTests = do
    onePosInt <- parseArgsTakesOnePosInt
    withMany <- parseArgsWithMany
    none <- parseArgsWithNone
    withString <- parseArgsWithString
    negative <- parseArgsNegativeInt
    return $ TestList [
        TestLabel "One positive int" onePosInt,
        TestLabel "With many args" withMany,
        TestLabel "With no arg" none,
        TestLabel "A string" withString,
        TestLabel "Negative number" negative
        ]

parseInputTests :: IO Test
parseInputTests = do
    one <- parseInputTakesOneDouble
    padding <- parseInputOneDoublePadding
    empty <- parseInputEmpty
    many <- parseInputMany
    return $ TestList [
        TestLabel "One number" one,
        TestLabel "One number with padding" padding,
        TestLabel "Empty input" empty,
        TestLabel "Many numbers" many
        ]

testSuites :: IO Test
testSuites = do
    args <- parseArgsTests
    input <- parseInputTests
    return $ TestList [
        TestLabel "Args" args,
        TestLabel "Input" input
        ]
