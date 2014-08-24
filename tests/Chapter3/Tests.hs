module Chapter3.Tests (allTests) where

import Chapter3.Golf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

allTests :: TestTree
allTests = testGroup "Chapter3 tests" [skipUnitTests, skipPropertyTests, localMaximaUnitTests]

skipUnitTests ::  TestTree
skipUnitTests =  testGroup "unit tests for skip" [
                testCase "emptyList" skips1,
                testCase "singletonList" skips2,
                testCase "listWithFourCharacters" skips3,
                testCase "listWithSixCharacters" skips4,
                testCase "listTrueAndFalse" skips5
            ]

localMaximaUnitTests :: TestTree
localMaximaUnitTests = testGroup "unit tests for localMaxima" [
                testCase "withTwoLocalMaxima" localMaxima1,
                testCase "withOneLocalMaxima" localMaxima2,
                testCase "withNoLocalMaxima" localMaxima3
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

localMaxima1 :: Assertion
localMaxima1 = localMaxima [2,9,5,6,1] @?= [9,6]

localMaxima2 :: Assertion
localMaxima2 = localMaxima [2,3,4,1,5] @?= [4]

localMaxima3 :: Assertion
localMaxima3 = localMaxima [1,2,3,4,5] @?= []

skipPropertyTests :: TestTree
skipPropertyTests = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "length xs == length (skips xs)" $
      \xs -> length (xs :: [Int]) == length (skips xs)]