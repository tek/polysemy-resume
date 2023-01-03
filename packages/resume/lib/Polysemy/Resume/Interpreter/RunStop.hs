{-# options_haddock prune #-}

-- |Description: Interpreters for 'RunStop'.
module Polysemy.Resume.Interpreter.RunStop where

import Polysemy.Resume.Effect.RunStop (RunStop, unRunStop)
import Polysemy.Resume.Interpreter.Stop (runStopPure, stopToIOFinal)

interpretRunStop :: InterpreterFor RunStop r
interpretRunStop =
  runScoped1 (const runStopPure) . rewrite unRunStop

interpretRunStopIO ::
  Member (Final IO) r =>
  InterpreterFor RunStop r
interpretRunStopIO =
  runScoped1 (const stopToIOFinal) . rewrite unRunStop
