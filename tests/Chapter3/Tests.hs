module Chapter3.Tests (allTests) where

import Chapter3.Golf
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

allTests :: TestTree
allTests = testGroup "Chapter3 tests" [skipUnitTests, skipPropertyTests, localMaximaUnitTests, historGramUnitTests]

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
                testCase "withNoLocalMaxima" localMaxima3,
                testCase "withEmptyList" localMaxima4
            ]

historGramUnitTests :: TestTree
historGramUnitTests = testGroup "unit tests for histogram" [
                    testCase "with [3,5]" histogram1,
                    testCase "with [1,1,1,5]" histogram2,
                    testCase "with [1,4,5,4,6,6,3,4,2,4,9]" histogram3
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

localMaxima4 :: Assertion
localMaxima4 = localMaxima [] @?= []

skipPropertyTests :: TestTree
skipPropertyTests = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "length xs == length (skips xs)" $
      \xs -> length (xs :: [Int]) == length (skips xs)]

histogram1 :: Assertion
histogram1 = histogram [3,5] @?=  "   * *    \n==========\n0123456789\n"

histogram2 :: Assertion
histogram2 = histogram [1,1,1,5] @?= " *        \n *        \n *   *    \n==========\n0123456789\n"

histogram3 :: Assertion
histogram3 = histogram [1,4,5,4,6,6,3,4,2,4,9] @?= "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"