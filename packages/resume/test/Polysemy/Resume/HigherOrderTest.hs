{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Resume.HigherOrderTest where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Resume (type (!!))
import Polysemy.Resume.Data.Stop (stop)
import Polysemy.Resume.Resumable (interpretResumableH)
import Polysemy.Resume.Resume (resume)

data Eff :: Effect where
  Nest :: m a -> Eff m a
  Result :: Eff m Int

makeSem ''Eff

data Off :: Effect where
  Off :: Off m Int

makeSem ''Off

interpretEffNested ::
  InterpreterFor (Eff !! Text) r
interpretEffNested =
  interpretResumableH \case
    Nest _ ->
      stop "no"
    Result ->
      pureT 2

interpretEff ::
  InterpreterFor (Eff !! Text) r
interpretEff =
  interpretResumableH \case
    Nest ma -> do
      sem <- runT ma
      raise (interpretEffNested sem)
    Result ->
      pureT 1

interpretEffFinally ::
  Members [Resource, AtomicState Int] r =>
  InterpreterFor (Eff !! Text) r
interpretEffFinally sem =
  interpretEff (finally sem log)
  where
    log =
      atomicModify (1 +)

-- interpretOff ::
--   Member (Eff !! Text) r =>
--   InterpreterFor (Off !! Text) r
-- interpretOff sem =
--   restop (nest $ interpretResumable (\Off -> result) (raiseUnder sem))

test_switchInterpreter :: UnitTest
test_switchInterpreter =
  runTestAuto do
    tv <- embed (newTVarIO 0)
    (2 ===) =<< runAtomicStateTVar tv (interpretEffFinally (resume (nest (result >> result)) \ _ -> pure 3))
    (1 ===) =<< embed (readTVarIO tv)
    (3 ===) =<< interpretEff (resume (nest (nest result)) \ _ -> pure 3)
    (1 ===) =<< interpretEff (resume result \ _ -> pure 3)
    -- (2 ===) =<< interpretEff (interpretOff (resume off \ _ -> pure 3))
