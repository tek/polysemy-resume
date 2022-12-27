{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Resume.Test.InterceptTest where

import Control.Concurrent.STM (newTVarIO)
import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Resume.Effect.Resumable (type (!!))
import Polysemy.Resume.Effect.Stop (stop)
import Polysemy.Resume.Interpreter.Resumable (interceptResumable, interpretResumable)
import Polysemy.Resume.Resume (restop, (!!), (<!))

data A :: Effect where
  A1 :: A m Int
  A2 :: A m Int
  A3 :: A m Int

makeSem ''A

interpretA ::
  Member (AtomicState Int) r =>
  InterpreterFor (A !! Int) r
interpretA =
  interpretResumable \case
    A1 ->
      atomicGet >>= \case
        5 -> stop 13
        n -> pure (n - 1000)
    A2 ->
      pure (-1)
    A3 ->
      pure (-1)

interceptA1 ::
  Members [A !! Int, AtomicState Int] r =>
  Sem r a ->
  Sem r a
interceptA1 =
  interceptResumable \case
    A1 ->
      atomicModify' (8 *) *> restop a1
    A2 -> do
      atomicPut 5
      restop a1
    A3 ->
      stop (23)

interceptA2 ::
  Members [A !! Int, AtomicState Int] r =>
  Sem r a ->
  Sem r a
interceptA2 =
  interceptResumable \case
    A1 ->
      atomicModify' (500 +) *> restop a1
    A2 ->
      restop a2
    A3 ->
      99 <! a3

test_intercept :: UnitTest
test_intercept =
  runTestAuto do
    tv <- embed (newTVarIO 0)
    i <- runAtomicStateTVar tv $ interpretA $ interceptA1 $ interceptA2 do
      i1 <- a1 !! \ i -> fail ("first failed with: " <> show i)
      i2 <- a2 !! pure
      i3 <- a3 !! pure
      pure (i1, i2, i3)
    (3000, 13, 99) === i
