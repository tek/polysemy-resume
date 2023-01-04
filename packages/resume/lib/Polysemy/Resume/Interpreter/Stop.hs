{-# options_haddock prune #-}

-- |Description: Interpreters for 'Stop'.
module Polysemy.Resume.Interpreter.Stop where

import qualified Control.Exception as Base
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Unique (Unique, hashUnique, newUnique)
import GHC.Exts (Any)
import Polysemy.Error (Error (Throw))
import Polysemy.Final (controlFinal, interpretFinal)
import Polysemy.Internal (Sem (Sem))
import Polysemy.Internal.Union (Weaving (Weaving), decomp, liftHandlerWithNat)
import qualified Text.Show
import Unsafe.Coerce (unsafeCoerce)

import Polysemy.Resume.Effect.RunStop (RunStop (RunStop))
import Polysemy.Resume.Effect.Stop (Stop (Stop), stop)

-- |Equivalent of 'runError'.
runStopPure ::
  Sem (Stop err : r) a ->
  Sem r (Either err a)
runStopPure (Sem m) =
  Sem \ k ->
    runExceptT $ m \ u ->
      case decomp u of
        Left x ->
          liftHandlerWithNat (ExceptT . runStopPure) k x
        Right (Weaving (Stop err) _ _ _) ->
          throwE err

-- | Internal type used to tag exceptions thrown by 'Stop' interpreters.
data StopExc = StopExc !Unique Any

instance Show StopExc where
  show (StopExc uid _) =
    "stopToIOFinal: Escaped opaque exception. Unique hash is: " <>
    show (hashUnique uid) <> "This should only happen if the computation that " <>
    "threw the exception was somehow invoked outside of the argument of 'stopToIOFinal'; " <>
    "for example, if you 'async' an exceptional computation inside of the argument " <>
    "provided to 'stopToIOFinal', and then 'await' on it *outside* of the argument " <>
    "provided to 'stopToIOFinal'. If that or any similar shenanigans seems unlikely, " <>
    "please open an issue on the GitHub repository."

instance Exception StopExc

runStopAsExcFinal ::
  ∀ e r a .
  Member (Final IO) r =>
  Unique ->
  Sem (Stop e : r) a ->
  Sem r a
runStopAsExcFinal uid =
  interpretFinal @IO \ (Stop e) ->
    embed (throwIO (StopExc uid (unsafeCoerce e)))

catchWithUid :: forall e a. Unique -> IO a -> (e -> IO a) -> IO a
catchWithUid uid m h = Base.catch m $ \exc@(StopExc uid' e) ->
  if uid == uid' then h (unsafeCoerce e) else throwIO exc
{-# inline catchWithUid #-}

stopToIOFinal ::
  ∀ e r a .
  Member (Final IO) r =>
  Sem (Stop e ': r) a ->
  Sem r (Either e a)
stopToIOFinal sem =
  controlFinal \ lower -> do
    uid <- newUnique
    catchWithUid @e uid (lower (Right <$> runStopAsExcFinal uid sem)) \ e ->
      lower (pure (Left e))
{-# inline stopToIOFinal #-}

-- |Stop if the argument is 'Left', transforming the error with @f@.
stopEitherWith ::
  ∀ err err' r a .
  Member (Stop err') r =>
  (err -> err') ->
  Either err a ->
  Sem r a
stopEitherWith f =
  either (stop . f) pure
{-# inline stopEitherWith #-}

-- |Stop if the argument is 'Left', using the supplied error.
stopEitherAs ::
  ∀ err err' r a .
  Member (Stop err') r =>
  err' ->
  Either err a ->
  Sem r a
stopEitherAs err =
  stopEitherWith (const err)
{-# inline stopEitherAs #-}

-- |Stop if the argument is 'Left'.
stopEither ::
  ∀ err r a .
  Member (Stop err) r =>
  Either err a ->
  Sem r a
stopEither =
  stopEitherWith id
{-# inline stopEither #-}

-- |Stop with the supplied error if the argument is 'Nothing'.
stopNote ::
  ∀ err r a .
  Member (Stop err) r =>
  err ->
  Maybe a ->
  Sem r a
stopNote err =
  maybe (stop err) pure
{-# inline stopNote #-}

-- |Convert a program using regular 'Error's to one using 'Stop'.
stopOnError ::
  ∀ err r a .
  Member (Stop err) r =>
  Sem (Error err : r) a ->
  Sem r a
stopOnError =
  stopEither <=< runError
{-# inline stopOnError #-}

-- |Convert a program using regular 'Error's to one using 'Stop'.
stopOnErrorWith ::
  ∀ err err' r a .
  Member (Stop err') r =>
  (err -> err') ->
  Sem (Error err : r) a ->
  Sem r a
stopOnErrorWith f =
  stopEitherWith f <=< runError
{-# inline stopOnErrorWith #-}

-- |Convert a program using 'Stop' to one using 'Error', transforming the error with the supplied function.
stopToErrorWith ::
  ∀ err err' r a .
  Member (Error err') r =>
  (err -> err') ->
  Sem (Stop err : r) a ->
  Sem r a
stopToErrorWith f =
  transform \ (Stop err) -> Throw (f err)
{-# inline stopToErrorWith #-}

-- |Convert a program using 'Stop' to one using 'Error'.
stopToError ::
  ∀ err r a .
  Member (Error err) r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToError =
  stopToErrorWith id
{-# inline stopToError #-}

-- |Map over the error type in a 'Stop'.
mapStop ::
  ∀ err e' r a .
  Member (Stop e') r =>
  (err -> e') ->
  Sem (Stop err : r) a ->
  Sem r a
mapStop f =
  transform \ (Stop err) -> Stop (f err)
{-# inline mapStop #-}

-- |Replace the error in a 'Stop' with another type.
replaceStop ::
  ∀ err e' r a .
  Member (Stop e') r =>
  e' ->
  Sem (Stop err : r) a ->
  Sem r a
replaceStop e' =
  mapStop (const e')
{-# inline replaceStop #-}

-- |Convert the error type in a 'Stop' to 'Text'.
showStop ::
  ∀ err r a .
  Show err =>
  Member (Stop Text) r =>
  Sem (Stop err : r) a ->
  Sem r a
showStop =
  mapStop @err @Text show
{-# inline showStop #-}

-- |Convert an 'IO' exception to 'Stop' using the provided transformation.
stopTryIOE ::
  ∀ exc err r a .
  Exception exc =>
  Members [Stop err, Embed IO] r =>
  (exc -> err) ->
  IO a ->
  Sem r a
stopTryIOE f =
  stopEitherWith f <=< tryIOE @exc
{-# inline stopTryIOE #-}

-- |Convert an 'IO' exception of type @err@ to 'Stop' using the provided transformation from 'Text'.
stopTryIO ::
  ∀ exc err r a .
  Exception exc =>
  Members [Stop err, Embed IO] r =>
  (Text -> err) ->
  IO a ->
  Sem r a
stopTryIO f =
  stopEitherWith f <=< tryIO @exc
{-# inline stopTryIO #-}

-- |Convert an 'IO' exception of type 'Control.Exception.IOError' to 'Stop' using the provided transformation from
-- 'Text'.
stopTryIOError ::
  ∀ err r a .
  Members [Stop err, Embed IO] r =>
  (Text -> err) ->
  IO a ->
  Sem r a
stopTryIOError f =
  stopEitherWith f <=< tryIOError
{-# inline stopTryIOError #-}

-- |Convert an 'IO' exception to 'Stop' using the provided transformation from 'Text'.
stopTryAny ::
  ∀ err r a .
  Members [Stop err, Embed IO] r =>
  (Text -> err) ->
  IO a ->
  Sem r a
stopTryAny f =
  stopEitherWith f <=< tryAny
{-# inline stopTryAny #-}

runStop ::
  ∀ err r a .
  Member RunStop r =>
  Sem (Stop err : r) a ->
  Sem r (Either err a)
runStop =
  transform RunStop . scoped1 (Const ()) . raiseUnder
