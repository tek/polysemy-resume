{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Resume.Test.ScopedTest where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Polysemy.Membership (Raise)
import Polysemy.Opaque (Opaque)
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Polysemy.Resume.Effect.Resumable (type (!!))
import Polysemy.Resume.Effect.RunStop (RunStop)
import Polysemy.Resume.Effect.Stop (Stop, stop)
import Polysemy.Resume.Interpreter.Resumable (interpretResumableH, runResumable)
import Polysemy.Resume.Interpreter.RunStop (interpretRunStop)
import Polysemy.Resume.Interpreter.Scoped (
  interpretResumableScopedH,
  interpretScopedResumableH,
  interpretScopedResumableWithH,
  )
import Polysemy.Resume.Resume (resumeAs, resuming, (!!))

newtype Par =
  Par { unPar :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data E :: Effect where
  E1 :: E m Int
  E2 :: E m Int

makeSem ''E

data F :: Effect where
  F :: F m Int

makeSem ''F

type Handler e r =
  ∀ z x t . e z x -> Sem (HigherOrder z t e r r : r) x

handleE ::
  Member (Embed IO) r =>
  TVar Int ->
  Handler E (F : r)
handleE tv = \case
  E1 -> do
    i1 <- embed (readTVarIO tv)
    i2 <- f
    pure (i1 + i2 + 10)
  E2 ->
    pure (-1)

interpretF ::
  Member (Embed IO) r =>
  TVar Int ->
  InterpreterFor F r
interpretF tv =
  interpret \ F -> do
    embed (atomically (writeTVar tv 7))
    pure 5

scope ::
  Member (Embed IO) r =>
  Par ->
  (TVar Int -> Sem (F : r) a) ->
  Sem r a
scope (Par n) use = do
  tv <- embed (newTVarIO n)
  interpretF tv (use tv)

data H1 :: Effect where
  H1a :: m Int -> H1 m Int
  H1b :: H1 m Int
  H1c :: H1 m Int
  H1d :: m Int -> H1 m Int

makeSem ''H1

scopeH1 :: Int -> (Int -> Sem (Opaque q : Stop Int : r) a) -> Sem (Opaque q : Stop Int : r) a
scopeH1 p use =
  if p == (-1) then stop 500000 else use (p + 5)

handleH1 ::
  Member (Stop Int) r =>
  Int ->
  Handler H1 r
handleH1 n = \case
  H1a ma -> do
    i <- runH ma
    pure (i + n + 1)
  H1b ->
    pure 50000
  H1c ->
    stop 100000
  H1d ma ->
    interpretH (handleH1 (n + 1)) (runH' ma)

test_scopedResumable :: UnitTest
test_scopedResumable =
  runTestAuto $ interpretRunStop $ interpretScopedResumableH @H1 @Int @Int @Int scopeH1 (\ x -> handleH1 x) do
    (i1, i2, i3, i4, i5) <- resumeAs @Int @(Scoped_ H1 Int) (-50, -100, -200, -500, -700) $ scoped_ @H1 @Int 20 do
      i1 <- h1a do
        h1b
      i2 <- resumeAs @Int @(Scoped_ H1 Int) (-1000) $ scoped_ @H1 @Int 23 do
        h1a do
          h1b
      i3 <- resuming pure $ scoped_ @H1 @Int 5000 do
        h1a do
          h1c
      i4 <- resuming pure $ scoped_ @H1 @Int (-1) do
        h1b
      i5 <- h1d do
        h1a h1b
      pure (i1, i2, i3, i4, i5)
    50026 === i1
    50029 === i2
    100000 === i3
    500000 === i4
    50027 === i5

handleRE ::
  ∀ q r .
  Member (Embed IO) r =>
  TVar Int ->
  Handler E (F : Opaque q : Stop Int : r)
handleRE tv = \case
  E1 -> do
    i1 <- embed (readTVarIO tv)
    i2 <- f
    _ <- stop (i1 + i2)
    pure (i1 + i2 + 10)
  E2 ->
    pure =<< f

scopeR ::
  Member (Embed IO) r =>
  Par ->
  (TVar Int -> Sem (F : Opaque q : Stop Int : r) a) ->
  Sem (Opaque q : Stop Int : r) a
scopeR (Par n) use = do
  tv <- embed (newTVarIO n)
  _ <- interpretF tv (use tv)
  stop . (50 +) =<< embed (readTVarIO tv)

test_scopedResumableWith :: UnitTest
test_scopedResumableWith =
  runTestAuto $ interpretRunStop $ interpretScopedResumableWithH @'[F] scopeR (\ x -> handleRE x) do
    i1 <- scoped_ 20 e1 !! pure
    i2 <- scoped_ 23 e2 !! pure
    25 === i1
    57 === i2

data Rs :: Effect where
  Rs1 :: m Int -> Rs m Int
  Rs2 :: Rs m Int
  Rs3 :: m Int -> Rs m Int
  Rs4 :: Rs m Int

makeSem ''Rs

scopeRs :: Int -> (Int -> Sem r a) -> Sem r a
scopeRs n use =
  use (n + 1)

handleRs ::
  Raise rPre r =>
  Member (Stop Int) r =>
  Int ->
  Rs z x ->
  Sem (HigherOrder z t Rs rPre r : r) x
handleRs n = \case
  Rs1 ma -> interpretH (handleRs (n + 1000000)) (runH' ma)
  Rs2 -> pure (n + 20)
  Rs3 _ -> stop 200
  Rs4 -> stop 2000

test_resumableScoped :: UnitTest
test_resumableScoped =
  runTestAuto $ interpretRunStop $ interpretResumableScopedH @Int @Int @Rs @Int scopeRs (\ x -> handleRs x) do
    (i1, i2, i3, i4, i5) <- scoped_ 10000 do
      i1 <- resuming pure $ rs1 do
        rs2
      i2 <- resuming pure $ rs3 do
        rs4
      i3 <- rs2 !! pure
      i4 <- resuming pure $ rs1 do
        rs4
      i5 <- raise $ scoped_ @(Rs !! Int) @Int 100000 do
        resumeAs 1000 $ rs1 rs2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

data Rsw :: Effect where
  Rsw1 :: m Int -> Rsw m Int
  Rsw2 :: Rsw m Int
  Rsw3 :: m Int -> Rsw m Int
  Rsw4 :: Rsw m Int

makeSem ''Rsw

data RswExtra :: Effect where
  RswExtra :: RswExtra m Int

makeSem ''RswExtra

scopeRsw :: Int -> (Int -> Sem (RswExtra : r) a) -> Sem r a
scopeRsw n use =
  interpret (\ RswExtra -> pure 20) $
  use (n + 1)

handleRsw ::
  Raise rPre r =>
  Members [RswExtra, Stop Int] r =>
  Int ->
  Rsw z x ->
  Sem (HigherOrder z t Rsw rPre r : r) x
handleRsw n = \case
  Rsw1 ma -> interpretH (handleRsw (n + 1000000)) (runH' ma)
  Rsw2 -> pure . (n +) =<< rswExtra
  Rsw3 _ -> stop 200
  Rsw4 -> stop 2000

interpretRsw ::
  Member RunStop r =>
  Int ->
  InterpreterFor (Rsw !! Int) r
interpretRsw n0 =
  interpret (\ RswExtra -> pure 20) .
  interpretResumableH (handleRsw (n0 + 1)) .
  raiseUnder @RswExtra

test_resumableScopedWith :: UnitTest
test_resumableScopedWith =
  runTestAuto $ interpretRunStop $ runScoped_ (\ x -> interpretRsw x) do
    (i1, i2, i3, i4, i5) <- scoped_ 10000 do
      i1 <- resuming pure $ rsw1 do
        rsw2
      i2 <- resuming pure $ rsw3 do
        rsw4
      i3 <- rsw2 !! pure
      i4 <- resuming pure $ rsw1 do
        rsw4
      i5 <- raise $ scoped_ @(Rsw !! Int) 100000 do
        resumeAs 1000 $ rsw1 rsw2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

data Rsr :: Effect where
  Rsr1 :: m Int -> Rsr m Int
  Rsr2 :: Rsr m Int
  Rsr3 :: m Int -> Rsr m Int
  Rsr4 :: Rsr m Int

makeSem ''Rsr

handleRsr ::
  Raise rPre r =>
  Member (Stop Int) r =>
  Int ->
  Rsr z x ->
  Sem (HigherOrder z t Rsr rPre r : r) x
handleRsr n = \case
  Rsr1 ma -> interpretH (handleRsr (n + 1000000)) (runH' ma)
  Rsr2 -> pure (n + 20)
  Rsr3 _ -> stop 200
  Rsr4 -> stop 2000

test_scopedR :: UnitTest
test_scopedR =
  runTestAuto $ interpretRunStop $ runResumable (runScoped_ (\ n -> interpretResumableH (handleRsr (n + 1)))) do
    (i1, i2, i3, i4, i5) <- resuming (\ i -> pure (round @Double i, round i, round i, round i, round i)) $ scoped_ 10000 do
      i1 <- resuming pure $ rsr1 do
        rsr2
      i2 <- resuming pure $ rsr3 do
        rsr4
      i3 <- rsr2 !! pure
      i4 <- resuming pure $ rsr1 do
        rsr4
      i5 <- raise $ scoped_ @(Rsr !! Int) @Int 100000 do
        resumeAs 1000 $ rsr1 rsr2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

scopeRsrw :: Int -> (Int -> Sem (RswExtra : Stop Double : r) a) -> Sem (Stop Double : r) a
scopeRsrw n use =
  interpret (\ RswExtra -> pure 20) $
  use (n + 1)

handleRsrw ::
  Member (Stop Int) r =>
  Raise rPre r =>
  Int ->
  Rsr z x ->
  Sem (HigherOrder z t Rsr rPre r : r) x
handleRsrw n = \case
  Rsr1 ma -> interpretH (handleRsr (n + 1000000)) (runH' ma)
  Rsr2 -> pure (n + 20)
  Rsr3 _ -> stop 200
  Rsr4 -> stop 2000

interpretRsrw ::
  Member RunStop r =>
  Int ->
  InterpreterFor (Rsr !! Int) r
interpretRsrw n =
  interpret (\ RswExtra -> pure 20) .
  interpretResumableH (handleRsrw (n + 1)) .
  raiseUnder @RswExtra

test_scopedRWith :: UnitTest
test_scopedRWith =
  runTestAuto $ interpretRunStop $ runResumable (runScoped_ (\ x -> interpretRsrw x)) do
    (i1, i2, i3, i4, i5) <- resuming (\ i -> pure (round @Double i, round i, round i, round i, round i)) $ scoped_ 10000 do
      i1 <- resuming pure $ rsr1 do
        rsr2
      i2 <- resuming pure $ rsr3 do
        rsr4
      i3 <- rsr2 !! pure
      i4 <- resuming pure $ rsr1 do
        rsr4
      i5 <- raise $ scoped_ @(Rsr !! Int) @Int 100000 do
        resumeAs 1000 $ rsr1 rsr2
      pure (i1, i2, i3, i4, i5)
    1010021 === i1
    200 === i2
    10021 === i3
    2000 === i4
    1100021 === i5

test_scoped :: TestTree
test_scoped =
  testGroup "scoped" [
    unitTest "scopedResumable" test_scopedResumable,
    unitTest "scopedResumableWith" test_scopedResumableWith,
    unitTest "resumableScoped" test_resumableScoped,
    unitTest "resumableScopedWith" test_resumableScopedWith,
    unitTest "scopedR" test_scopedR,
    unitTest "scopedRWith" test_scopedRWith
  ]
