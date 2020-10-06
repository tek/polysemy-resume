module Main where

import Polysemy.Resume.ExampleTest (test_example)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "unit" [
    unitTest "stop and resume" test_example
  ]

main :: IO ()
main =
  defaultMain tests
