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

displayValue :: String -> Maybe Double -> String -> IO ()
displayValue pre Nothing suf = printf "%s=nan%s" pre suf
displayValue pre (Just v) suf = printf "%s=%.2f%s" pre v suf

output :: Int -> [Double] -> IO ()
output period temps =
    let
        increase = avgTempIncreases period temps
    in
    displayValue "g" increase "" >>
    displayValue "r" increase "" >>
    displayValue "s" increase "" >>
    putStrLn ""
