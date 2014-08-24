module Chapter3.Golf (skips, localMaxima) where

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