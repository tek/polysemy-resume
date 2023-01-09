{-# options_haddock prune #-}

-- |Description: Interpreters for 'RunStop'.
module Polysemy.Resume.Interpreter.RunStop where

import Polysemy.HigherOrder (restoreH)
import Polysemy.Meta (interpretMeta, runExposeMeta)
import Polysemy.Newtype (coerceEff)
import Polysemy.Opaque (Opaque, fromOpaque, toOpaqueAt)

import Polysemy.Resume.Effect.RunStop (RunStop (RunStop), RunStopMeta (RunStopMeta))
import Polysemy.Resume.Effect.Stop (Stop)
import Polysemy.Resume.Interpreter.Stop (runStopPure, stopToIOFinal)

customRunStop ::
  (∀ e q x . Sem (Stop e : Opaque q : r) x -> Sem (Opaque q : r) (Either e x)) ->
  InterpreterFor RunStop r
customRunStop interp =
  coerceEff
  >>> interpretMeta @RunStopMeta \case
    RunStopMeta m ->
      runExposeMeta (fromOpaque . interp . toOpaqueAt @'[_]) m >>= \case
        Left e -> pure (Left e)
        Right ta -> Right <$> restoreH ta

interpretRunStop :: InterpreterFor RunStop r
interpretRunStop =
  customRunStop runStopPure

interpretRunStopIO ::
  Member (Final IO) r =>
  InterpreterFor RunStop r
interpretRunStopIO =
  customRunStop stopToIOFinal
