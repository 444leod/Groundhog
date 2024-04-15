{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Output
-}

module Output (
    output,
    avgTempIncreases
) where

import Text.Printf

positiveElevations :: [Double] -> [Double]
positiveElevations [] = []
positiveElevations [_] = []
positiveElevations (x:y:xs)
    | x > y = (x - y):rest
    | otherwise = 0:rest
    where rest = positiveElevations (y:xs)

avgTempIncreases :: Int -> [Double] -> Maybe Double
avgTempIncreases period temps
    | period > length incs = Nothing
    | otherwise = Just $ (sum incs) / (fromIntegral $ length incs)
    where incs = positiveElevations $ take (period + 1) temps

relativeEvolution :: Int -> [Double] -> Maybe Int
relativeEvolution period temps
    | period >= length temps = Nothing
    | otherwise = Just $ (round :: Double -> Int) $
        (head temps / (temps !! period) - 1) * 100

displayDoubleValue :: String -> Maybe Double -> IO ()
displayDoubleValue pre Nothing = printf "%s=nan" pre
displayDoubleValue pre (Just v) = printf "%s=%.2f" pre v

displayPercValue :: String -> Maybe Int -> IO ()
displayPercValue pre Nothing = printf "%s=nan%%" pre
displayPercValue pre (Just v) = printf "%s=%d%%" pre v

output :: Int -> [Double] -> IO ()
output period temps =
    let
        increase = avgTempIncreases period temps
        evolution = relativeEvolution period temps
    in
    displayDoubleValue "g" increase >> putStr "\t" >>
    displayPercValue "r" evolution >> putStr "\t" >>
    displayDoubleValue "s" increase >> putStrLn ""
