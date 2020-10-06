module Main where

import Polysemy.Resume.ResumeTest (test_resume)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "unit" [
    unitTest "stop and resume" test_resume
  ]

main :: IO ()
main =
  defaultMain tests
