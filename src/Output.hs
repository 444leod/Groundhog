{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Output
-}

module Output (
    avgTempIncreases
) where

positiveElevations :: [Double] -> [Double]
positiveElevations [] = []
positiveElevations [_] = []
positiveElevations (x:y:xs)
    | x > y = (x - y):rest
    | otherwise = 0:rest
    where rest = positiveElevations (y:xs)

avgTempIncreases :: Int -> [Double] -> Double
avgTempIncreases period temps = (sum incs) / (fromIntegral $ length incs)
    where incs = positiveElevations $ take period temps
