module Chapter3.Golf (skips, filterSkip) where

skips :: [a] -> [[a]]
skips [] =  [[]]
skips xs = map ((flip filterSkip) xs) [1..(length xs)]


filterSkip :: Int -> [a] -> [a]
filterSkip n xs =
    let pairs = zip [1..] xs
        matches = filter (\(i,_) -> i `mod` n == 0) pairs
    in  map snd matches