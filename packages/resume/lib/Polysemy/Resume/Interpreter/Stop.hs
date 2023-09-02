{-# options_haddock prune #-}

-- |Description: Interpreters for 'Stop'.
module Polysemy.Resume.Interpreter.Stop where

import qualified Control.Exception as Base
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE)
import Data.Typeable (typeRep)
import Polysemy.Final (getInitialStateS, interpretFinal, runS, withStrategicToFinal)
import Polysemy.Internal (Sem (Sem), usingSem)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, weave)
import qualified Text.Show

import Polysemy.Resume.Effect.Stop (Stop (Stop), stop)

-- |Equivalent of 'runError'.
runStop ::
  Sem (Stop err : r) a ->
  Sem r (Either err a)
runStop (Sem m) =
  Sem \ k ->
    runExceptT $ m \ u ->
      case decomp u of
        Left x ->
          ExceptT $ k $ weave (Right ()) (either (pure . Left) runStop) rightToMaybe x
        Right (Weaving (Stop err) _ _ _ _) ->
          throwE err
{-# inline runStop #-}

-- | Internal type used to tag exceptions thrown by 'Stop' interpreters.
newtype StopExc err =
  StopExc { unStopExc :: err }
  deriving stock (Typeable)

instance {-# overlappable #-} Typeable err => Show (StopExc err) where
  show =
    mappend "StopExc: " . show . typeRep

instance Show (StopExc Text) where
  show (StopExc err) =
    "StopExc " <> show err

instance {-# overlappable #-} Typeable err => Exception (StopExc err)

instance Exception (StopExc Text)

-- |Run 'Stop' by throwing exceptions.
runStopAsExcFinal ::
  ∀ err r a .
  Exception (StopExc err) =>
  Member (Final IO) r =>
  Sem (Stop err : r) a ->
  Sem r a
runStopAsExcFinal =
  interpretFinal \case
    Stop err ->
      pure (throwIO (StopExc err))
{-# inline runStopAsExcFinal #-}

-- |Run 'Stop' by throwing and catching exceptions.
stopToIOFinal ::
  ∀ err r a .
  Exception (StopExc err) =>
  Member (Final IO) r =>
  Sem (Stop err : r) a ->
  Sem r (Either err a)
stopToIOFinal sem =
  withStrategicToFinal @IO do
    m' <- runS (runStopAsExcFinal sem)
    s <- getInitialStateS
    pure $ either ((<$ s) . Left . \ (StopExc e) -> e) (fmap Right) <$> Base.try m'
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
  either (throw . f) pure <=< runStop
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

-- |Convert a program using 'Stop' to one using 'Error'.
stopToErrorIO ::
  ∀ err r a .
  Exception (StopExc err) =>
  Members [Error err, Final IO] r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToErrorIO =
  either throw pure <=< stopToIOFinal
{-# inline stopToErrorIO #-}

-- |Map over the error type in a 'Stop'.
mapStop ::
  ∀ err e' r a .
  Member (Stop e') r =>
  (err -> e') ->
  Sem (Stop err : r) a ->
  Sem r a
mapStop f (Sem m) =
  Sem \ k -> m \ u ->
    case decomp u of
      Left x ->
        k (hoist (mapStop f) x)
      Right (Weaving (Stop err) _ _ _ _) ->
        usingSem k (send $ Stop (f err))
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
