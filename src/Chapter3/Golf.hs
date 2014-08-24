module Chapter3.Golf (skips, filterSkip) where

skips :: [a] -> [[a]]
skips [] =  []
skips xs = map ((flip filterSkip) xs) [1..(length xs)]


filterSkip :: Int -> [a] -> [a]
filterSkip n = map snd . filter (\(i,_) -> i `mod` n == 0) . zip [1..]