module Chapter3.Golf (skips, localMaxima, histogram, row) where

import Data.List (intercalate)

skips :: [a] -> [[a]]
skips [] =  []
skips xs = map ((flip filterSkip) xs) [1..(length xs)]

filterSkip :: Int -> [a] -> [a]
filterSkip n = map snd . filter (\(i,_) -> i `mod` n == 0) . zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima  (b:e:a:xs)
    | e > b && e > a = e : localMaxima (a : xs)
    | otherwise = localMaxima (e : a : xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = let allRows = [ row (replicate 10 ' ') r | r <- rowOrder [[]] xs]
                   allRowsWithLegend = "" : numbers : underline : allRows
                   withNewLines = intercalate "\n" (reverse allRowsWithLegend)
                in withNewLines

-- creates  a single row
row :: String -> [Integer] -> String
row acc [] = acc
row acc (x:xs)  =
        let (before, after) = splitAt (fromIntegral x :: Int) acc
            newAcc = before ++ ('*' : tail after)
        in row newAcc xs

underline :: String
underline =  replicate 10 '='

numbers :: String
numbers = "0123456789"

rowOrder :: [[Integer]] -> [Integer] -> [[Integer]]
rowOrder  acc [] = acc
rowOrder acc (x:xs) = rowOrder (insertAt acc x) xs

-- find the correct place to insert the new integer
insertAt :: [[Integer]] -> Integer -> [[Integer]]
insertAt [] y = [[y]]
insertAt  (xx:xxs) y
    | all (/= y) xx = (y : xx) : xxs
    | otherwise = xx : insertAt xxs y