module Chapter3.Tests (unitTests, propertyTests) where

import Chapter3.Golf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

unitTests ::  TestTree
unitTests =  testGroup "unit tests for skip" [
                testCase "emptyList" skips1,
                testCase "singletonList" skips2,
                testCase "listWithFourCharacters" skips3,
                testCase "listWithSixCharacters" skips4,
                testCase "listTrueAndFalse" skips5
            ]


skips1 :: Assertion

skips1 = skips ([]::[Int]) @?= ([] :: [[Int]])

skips2 :: Assertion
skips2 =  skips ([1] :: [Int]) @?= ([[1]] :: [[Int]])

skips3 :: Assertion
skips3 = skips "ABCD" @?= ["ABCD", "BD", "C", "D"]

skips4  :: Assertion
skips4 =  skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"]

skips5 :: Assertion
skips5 =  skips [True, False] @?= [[True, False], [False]]

propertyTests :: TestTree
propertyTests = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "length xs == length (skips xs)" $
      \xs -> length (xs :: [Int]) == length (skips xs)]