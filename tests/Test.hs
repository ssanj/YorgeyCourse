module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Chapter3.Tests as Chap3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
            testCase "emptyList" Chap3.skips1,
            testCase "singletonList" Chap3.skips2,
            testCase "listWithFourCharacters" Chap3.skips3,
            testCase "listWithSixCharacters" Chap3.skips4,
            testCase "listTrueAndFalse" Chap3.skips5
        ]