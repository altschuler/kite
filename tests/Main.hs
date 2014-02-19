module Main where

import Test.Tasty
import Test.Tasty.HUnit

import LexerTest


main :: IO ()
main = defaultMain tests

tests = testGroup "Tests"
        [
          truthTest,
          lexerTests
        ]

truthTest = testCase "Truth" $
             1+1 @?= 3
