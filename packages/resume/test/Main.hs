module Main where

import Polysemy.Resume.ExampleTest (test_example)
import Polysemy.Resume.HigherOrderTest (test_switchInterpreter)
import Polysemy.Resume.Test.InterceptTest (test_intercept)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "unit" [
    unitTest "stop and resume" test_example,
    unitTest "switch higher order interpreter" test_switchInterpreter,
    unitTest "intercept" test_intercept
  ]

main :: IO ()
main =
  defaultMain tests
