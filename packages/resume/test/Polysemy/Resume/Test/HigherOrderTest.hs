{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Resume.Test.HigherOrderTest where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Resume.Effect.Resumable (type (!!))
import Polysemy.Resume.Effect.RunStop (RunStop)
import Polysemy.Resume.Effect.Stop (Stop, stop)
import Polysemy.Resume.Interpreter.Resumable (interpretResumableH)
import Polysemy.Resume.Interpreter.RunStop (interpretRunStop)
import Polysemy.Resume.Resume (resume)
import qualified Polysemy.HigherOrder.Flexible as Flex

data Eff :: Effect where
  Nest :: m a -> Eff m a
  Result :: Eff m Int

makeSem ''Eff

handleEffNested ::
  Member (Stop Text) r =>
  Eff z a ->
  Sem r a
handleEffNested = \case
  Nest _ ->
    stop "no"
  Result ->
    pure 2

interpretEff ::
  Member RunStop r =>
  InterpreterFor (Eff !! Text) r
interpretEff =
  interpretResumableH \case
    Nest ma -> do
      interpretH handleEffNested (Flex.runH' ma)
    Result ->
      pure 1

interpretEffFinally ::
  Member RunStop r =>
  Members [Bracket, AtomicState Int] r =>
  InterpreterFor (Eff !! Text) r
interpretEffFinally sem =
  interpretEff (finally sem log)
  where
    log =
      atomicModify (1 +)

test_switchInterpreter :: UnitTest
test_switchInterpreter =
  runTestAuto $ interpretRunStop do
    tv <- embed (newTVarIO 0)
    (2 ===) =<< runAtomicStateTVar tv (interpretEffFinally (resume (nest (result >> result)) \ _ -> pure 3))
    (1 ===) =<< embed (readTVarIO tv)
    (3 ===) =<< interpretEff (resume (nest (nest result)) \ _ -> pure 3)
    (1 ===) =<< interpretEff (resume result \ _ -> pure 3)
