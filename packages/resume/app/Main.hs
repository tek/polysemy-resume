{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Main where

import Polysemy.Resume.Effect.Resumable (type (!!))
import Polysemy.Resume.Effect.Stop (Stop, stop)
import Polysemy.Resume.Interpreter.Resumable (interpretResumableH, runResumable)
import Polysemy.Resume.Interpreter.RunStop (interpretRunStopIO)
import Polysemy.Resume.Resume (resumeAs, resuming, (!!))

data Rsr :: Effect where
  Rsr1 :: m Int -> Rsr m Int
  Rsr2 :: Rsr m Int
  Rsr3 :: m Int -> Rsr m Int
  Rsr4 :: Rsr m Int

makeSem ''Rsr

handleRsr ::
  Member (Stop Int) r =>
  Int ->
  Rsr z x ->
  Sem (HigherOrder z t Rsr r : r) x
handleRsr n = \case
  Rsr1 ma -> interpretH (handleRsr (n + 1000000)) (runH' ma)
  Rsr2 -> pure (n + 20)
  Rsr3 _ -> stop 200
  Rsr4 -> stop 2000

step :: Int -> IO ()
step 0 =
  unit
step n = do
  runM $ interpretRunStopIO $ runResumable (runScoped (\ i -> interpretResumableH (handleRsr (i + 1)))) do
    (!_, !_, !_, !_, !_) <- resuming (\ i -> pure (round @Double i, round i, round i, round i, round i)) $ scoped 10000 do
      i1 <- resuming pure $ rsr1 do
        rsr2
      i2 <- resuming pure $ rsr3 do
        rsr4
      i3 <- rsr2 !! pure
      i4 <- resuming pure $ rsr1 do
        rsr4
      i5 <- raise $ scoped @(Rsr !! Int) @Int 100000 do
        resumeAs 1000 $ rsr1 rsr2
      pure (i1, i2, i3, i4, i5)
    unit
  step (n - 1)

main :: IO ()
main =
  step 5
