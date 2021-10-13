module Polysemy.Resume.Stop where

import Control.Exception (throwIO, try)
import Control.Monad.Trans.Except (throwE)
import Data.Typeable (typeRep)
import Polysemy (Final, embed)
import Polysemy.Error (runError, throw)
import Polysemy.Final (controlF, interpretFinal)
import Polysemy.Internal (Sem (Sem), send, usingSem)
import Polysemy.Internal.Union (Weaving (Weaving), decomp, hoist, liftHandlerWithNat)
import qualified Text.Show

import Polysemy.Resume.Data.Stop (Stop (Stop), stop)

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left _) = Nothing

-- |Equivalent of 'runError'.
runStop ::
  Sem (Stop e : r) a ->
  Sem r (Either e a)
runStop (Sem m) =
  Sem \ k ->
    runExceptT $ m \ u ->
      case decomp u of
        Left x ->
          liftHandlerWithNat (ExceptT . runStop) k x
        Right (Weaving (Stop e) _ _ _) ->
          throwE e
{-# inline runStop #-}

newtype StopExc e =
  StopExc { unStopExc :: e }
  deriving (Typeable)

instance {-# overlappable #-} Typeable e => Show (StopExc e) where
  show =
    mappend "StopExc: " . show . typeRep

instance Show (StopExc Text) where
  show (StopExc e) =
    "StopExc " <> show e

instance {-# overlappable #-} Typeable e => Exception (StopExc e)

instance Exception (StopExc Text)

runStopAsExcFinal ::
  Exception (StopExc e) =>
  Member (Final IO) r =>
  Sem (Stop e : r) a ->
  Sem r a
runStopAsExcFinal =
  interpretFinal @IO \case
    Stop e ->
      embed (throwIO (StopExc e))
{-# inline runStopAsExcFinal #-}

-- |Run 'Stop' by throwing exceptions.
stopToIOFinal ::
  Exception (StopExc e) =>
  Member (Final IO) r =>
  Sem (Stop e : r) a ->
  Sem r (Either e a)
stopToIOFinal sem =
  controlF @IO \ lower ->
    try (lower (runStopAsExcFinal sem)) >>= \case
      Right ta ->
        pure (Right <$> ta)
      Left (StopExc e) ->
        lower (pure (Left e))
{-# inline stopToIOFinal #-}

-- |Stop if the argument is 'Left', transforming the error with @f@.
stopEitherWith ::
  Member (Stop err') r =>
  (err -> err') ->
  Either err a ->
  Sem r a
stopEitherWith f =
  either (stop . f) pure
{-# inline stopEitherWith #-}

-- |Stop if the argument is 'Left'.
stopEither ::
  Member (Stop err) r =>
  Either err a ->
  Sem r a
stopEither =
  stopEitherWith id
{-# inline stopEither #-}

-- |Stop with the supplied error if the argument is 'Nothing'.
stopNote ::
  Member (Stop err) r =>
  err ->
  Maybe a ->
  Sem r a
stopNote err =
  maybe (stop err) pure
{-# inline stopNote #-}

-- |Convert a program using regular 'Error's to one using 'Stop'.
stopOnError ::
  Member (Stop err) r =>
  Sem (Error err : r) a ->
  Sem r a
stopOnError =
  stopEither <=< runError
{-# inline stopOnError #-}

-- |Convert a program using regular 'Error's to one using 'Stop'.
stopOnErrorWith ::
  Member (Stop err') r =>
  (err -> err') ->
  Sem (Error err : r) a ->
  Sem r a
stopOnErrorWith f =
  stopEitherWith f <=< runError
{-# inline stopOnErrorWith #-}

-- |Convert a program using 'Stop' to one using 'Error'.
stopToError ::
  Member (Error err) r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToError =
  either throw pure <=< runStop
{-# inline stopToError #-}

-- |Convert a program using 'Stop' to one using 'Error'.
stopToErrorIO ::
  Exception (StopExc err) =>
  Members [Error err, Final IO] r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToErrorIO =
  either throw pure <=< stopToIOFinal
{-# inline stopToErrorIO #-}

-- |Map over the error type in a 'Stop'.
mapStop ::
  ∀ e e' r a .
  Member (Stop e') r =>
  (e -> e') ->
  Sem (Stop e : r) a ->
  Sem r a
mapStop f (Sem m) =
  Sem \ k -> m \ u ->
    case decomp u of
      Left x ->
        k (hoist (mapStop f) x)
      Right (Weaving (Stop e) _ _ _) ->
        usingSem k (send $ Stop (f e))
{-# inline mapStop #-}

-- |Convert the error type in a 'Stop' to 'Text'.
showStop ::
  ∀ e r a .
  Show e =>
  Member (Stop Text) r =>
  Sem (Stop e : r) a ->
  Sem r a
showStop =
  mapStop @e @Text show
{-# inline showStop #-}
