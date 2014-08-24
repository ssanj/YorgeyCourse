module Main where

import Test.Tasty
import qualified Chapter3.Tests as Chap3

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [
            Chap3.unitTests, Chap3.propertyTests
        ]