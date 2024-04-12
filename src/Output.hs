{-
-- EPITECH PROJECT, 2024
-- BsGroundhog
-- File description:
-- Output
-}

module Output (
    output
) where

-- Temperature Increase Average
tempIncreases :: Int -> [Double] -> [Double]
tempIncreases _ [] = []
tempIncreases _ [_] = []
tempIncreases 0 _ = []
tempIncreases s (x:y:xs)
    | x >= y = (x - y) : (tempIncreases (s - 1) $ y:xs)
    | otherwise = tempIncreases (s - 1) $ y:xs

daverage :: [Double] -> Double
daverage l = (foldl (+) 0.0 l) / (fromIntegral $ length l)

printTAIs :: Int -> [Double] -> IO ()
printTAIs p l = print $ daverage increases
    where increases = tempIncreases p l

output :: Int -> [Double] -> IO ()
output p l = printTAIs p l
