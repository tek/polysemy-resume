module Polysemy.Resume.Test.V2Test where

import Polysemy.Test (UnitTest, runTestAuto)

import Polysemy.Resume.Effect.Stop (Stop, stop)
import Polysemy.Resume.Interpreter.Resumable (interpretResumable)
import Polysemy.Resume.Interpreter.RunStop (interpretRunStopIO)
import Polysemy.Resume.Resume (resume)

data One :: Effect where
  One :: One m Int
  Two :: One m Int

makeSem ''One

handle ::
  Member (Stop Text) r =>
  One z a ->
  Sem r a
handle = \case
  One -> stop ("maaaaahhh" :: Text)
  Two -> pure 5

test_v2 :: UnitTest
test_v2 =
  runTestAuto $ interpretRunStopIO $ interpretResumable @Text @One handle do
    a <- resume @Text @One one \ e -> dbg e $> (1 :: Int)
    b <- resume @Text @One two \ e -> dbg e $> (1 :: Int)
    dbgs a
    dbgs b
