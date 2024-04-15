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

positiveElevations :: [Double] -> [Double]
positiveElevations [] = []
positiveElevations [_] = []
positiveElevations (x:y:xs)
    | x > y = (x - y):rest
    | otherwise = 0:rest
    where rest = positiveElevations (y:xs)

avgTempIncreases :: Int -> [Double] -> Maybe Double
avgTempIncreases period temps
    | period - 1 > length incs = Nothing
    | otherwise = Just $ (sum incs) / (fromIntegral $ length incs)
    where incs = positiveElevations $ take period temps

display :: [Maybe Double] -> IO ()
display [] = putStrLn ""
display [x] = print x >> putStrLn ""
display (x:xs) = print x >> putStr "\t" >> display (xs)

output :: Int -> [Double] -> IO ()
output period temps = do
    let increase = avgTempIncreases period temps
    display [increase]
