module Chapter3.Tests (skips1, skips2, skips3, skips4, skips5) where

import Chapter3.Golf
import Test.Tasty.HUnit


skips1 :: Assertion
skips1 = skips ([]::[Int]) @?= ([[]] :: [[Int]])

skips2 :: Assertion
skips2 =  skips ([1] :: [Int]) @?= ([[1]] :: [[Int]])

skips3 :: Assertion
skips3 = skips "ABCD" @?= ["ABCD", "BD", "C", "D"]

skips4  :: Assertion
skips4 =  skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"]

skips5 :: Assertion
skips5 =  skips [True, False] @?= [[True, False], [False]]