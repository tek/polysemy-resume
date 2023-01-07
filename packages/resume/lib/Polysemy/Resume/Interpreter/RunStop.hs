{-# options_haddock prune #-}

-- |Description: Interpreters for 'RunStop'.
module Polysemy.Resume.Interpreter.RunStop where

import Polysemy.Meta (interpretMeta, runMeta)
import Polysemy.Newtype (coerceEff)
import Polysemy.Opaque (Opaque, collectOpaqueBundleAt, runOpaqueBundleAt)

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
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp
      & runOpaqueBundleAt @0

interpretRunStop :: InterpreterFor RunStop r
interpretRunStop =
  customRunStop runStopPure

interpretRunStopIO ::
  Member (Final IO) r =>
  InterpreterFor RunStop r
interpretRunStopIO =
  customRunStop stopToIOFinal
