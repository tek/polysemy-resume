module Polysemy.Resume.Stop where

import Control.Exception (throwIO, try)
import Control.Monad.Trans.Except (throwE)
import Data.Typeable (typeRep)
import qualified Text.Show

import Polysemy (Final)
import Polysemy.Error (runError, throw)
import Polysemy.Final (getInitialStateS, interpretFinal, runS, withStrategicToFinal)
import Polysemy.Internal (Sem(Sem), send, usingSem)
import Polysemy.Internal.Union (Weaving(Weaving), decomp, hoist, weave)
import Polysemy.Resume.Data.Stop (Stop(Stop), stop)

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
          ExceptT $ k $ weave (Right ()) (either (pure . Left) runStop) hush x
        Right (Weaving (Stop e) _ _ _ _) ->
          throwE e
{-# INLINE runStop #-}

newtype StopExc e =
  StopExc { unStopExc :: e }
  deriving (Typeable)

instance {-# overlappable #-} Typeable e => Show (StopExc e) where
  show =
    mappend "StopExc: " . show . typeRep

instance Show (StopExc Text) where
  show (StopExc e) =
    "StopExc " <> show e

instance Typeable e => Exception (StopExc e)

runStopAsExcFinal ::
  Typeable e =>
  Member (Final IO) r =>
  Sem (Stop e : r) a ->
  Sem r a
runStopAsExcFinal =
  interpretFinal \case
    Stop e ->
      pure (throwIO (StopExc e))
{-# INLINE runStopAsExcFinal #-}

-- |Run 'Stop' by throwing exceptions.
stopToIOFinal ::
  Typeable e =>
  Member (Final IO) r =>
  Sem (Stop e : r) a ->
  Sem r (Either e a)
stopToIOFinal sem =
  withStrategicToFinal @IO do
    m' <- runS (runStopAsExcFinal sem)
    s <- getInitialStateS
    pure $ either ((<$ s) . Left . unStopExc) (fmap Right) <$> try m'
{-# INLINE stopToIOFinal #-}

-- |Stop if the argument is 'Left', transforming the error with @f@.
stopEitherWith ::
  Member (Stop err') r =>
  (err -> err') ->
  Either err a ->
  Sem r a
stopEitherWith f =
  either (stop . f) pure
{-# INLINE stopEitherWith #-}

-- |Stop if the argument is 'Left'.
stopEither ::
  Member (Stop err) r =>
  Either err a ->
  Sem r a
stopEither =
  stopEitherWith id
{-# INLINE stopEither #-}

-- |Stop with the supplied error if the argument is 'Nothing'.
stopNote ::
  Member (Stop err) r =>
  err ->
  Maybe a ->
  Sem r a
stopNote err =
  maybe (stop err) pure
{-# INLINE stopNote #-}

-- |Convert a program using regular 'Error's to one using 'Stop'.
stopOnError ::
  Member (Stop err) r =>
  Sem (Error err : r) a ->
  Sem r a
stopOnError =
  stopEither <=< runError
{-# INLINE stopOnError #-}

-- |Convert a program using 'Stop' to one using 'Error'.
stopToError ::
  Member (Error err) r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToError =
  either throw pure <=< runStop
{-# INLINE stopToError #-}

-- |Convert a program using 'Stop' to one using 'Error'.
stopToErrorIO ::
  Typeable err =>
  Members [Error err, Final IO] r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToErrorIO =
  either throw pure <=< stopToIOFinal
{-# INLINE stopToErrorIO #-}

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
      Right (Weaving (Stop e) _ _ _ _) ->
        usingSem k (send $ Stop (f e))
{-# INLINE mapStop #-}
