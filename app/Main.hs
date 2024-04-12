{-
-- EPITECH PROJECT, 2024
-- Groundhog
-- File description:
-- Main
-}

module Main (main) where

import Input
import Output
import Parameters

appendInput :: [Double] -> IO [Double]
appendInput l = do
    f <- getInput
    return $ f:l

mainLoop :: Int -> [Double] ->  IO ()
mainLoop p l = do
    values <- appendInput l
    output p values
    mainLoop p values

main :: IO ()
main = do
    period <- getPeriod
    mainLoop period []
