{-# OPTIONS_GHC -Wno-all #-}

module Polysemy.Resume.HigherOrderTest where

import Polysemy
import Polysemy.Resource
import Polysemy.State
import Polysemy.Resume (type (!))
import Polysemy.Resume.Data.Stop (Stop, stop)
import Polysemy.Resume.Resumable (catchResumable, resumable, interpretResumableH)
import Polysemy.Resume.Resume (restop, resume)
import Polysemy.Resume.Stop (runStop)
import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))
import Polysemy.AtomicState

data Eff :: Effect where
  Nest :: m a -> Eff m a
  Result :: Eff m Int

makeSem ''Eff

interpretEffNested ::
  InterpreterFor (Eff ! Text) r
interpretEffNested =
  interpretResumableH \case
    Nest _ ->
      stop "no"
    Result ->
      pureT 2

interpretEff ::
  InterpreterFor (Eff ! Text) r
interpretEff =
  interpretResumableH \case
    Nest ma -> do
      sem <- runT ma
      raise (interpretEffNested sem)
    Result ->
      pureT 1

interpretEffFinally ::
  Members [Resource, AtomicState Int] r =>
  InterpreterFor (Eff ! Text) r
interpretEffFinally sem =
  interpretEff (finally sem log)
  where
    log =
      atomicModify (1 +)

test_switchInterpreter :: UnitTest
test_switchInterpreter =
  runTestAuto do
    tv <- newTVarIO 0
    (2 ===) =<< runAtomicStateTVar tv (interpretEffFinally (resume (nest (result >> result)) \ _ -> pure 3))
    (1 ===) =<< readTVarIO tv
    (3 ===) =<< interpretEff (resume (nest (nest result)) \ _ -> pure 3)
    (1 ===) =<< interpretEff (resume (result) \ _ -> pure 3)
