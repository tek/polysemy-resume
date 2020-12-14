{-# options_ghc -Wno-all #-}

module Polysemy.Resume.HigherOrderTest where

import Polysemy (makeSem, pureT, raise, raiseUnder, runT)
import Polysemy.AtomicState (
  AtomicState,
  atomicModify,
  runAtomicStateTVar,
  )
import Polysemy.Resource (Resource, finally)
import Polysemy.Resume (type (!))
import Polysemy.Resume.Data.Stop (stop)
import Polysemy.Resume.Resumable (interpretResumable, interpretResumableH)
import Polysemy.Resume.Resume (restop, resume)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Resume.Data.Stop (Stop)
import Polysemy.Resume.Stop (stopNote)

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
    tv <- newTVarIO 0
    (2 ===) =<< runAtomicStateTVar tv (interpretEffFinally (resume (nest (result >> result)) \ _ -> pure 3))
    (1 ===) =<< readTVarIO tv
    (3 ===) =<< interpretEff (resume (nest (nest result)) \ _ -> pure 3)
    (1 ===) =<< interpretEff (resume result \ _ -> pure 3)
    -- (2 ===) =<< interpretEff (interpretOff (resume off \ _ -> pure 3))
