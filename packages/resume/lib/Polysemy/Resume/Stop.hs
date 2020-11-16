module Polysemy.Resume.Stop where

import qualified Text.Show
import Control.Monad.Trans.Except (throwE)
import Polysemy.Error (runError, throw)
import Polysemy.Internal (Sem(Sem))
import Polysemy.Internal.Union (Weaving(Weaving), decomp, weave)
import Polysemy (Final)
import Polysemy.Final (getInitialStateS, interpretFinal, runS, withStrategicToFinal)
import Data.Typeable (typeRep)

import Polysemy.Resume.Data.Stop (Stop(Stop), stop)
import Control.Exception (throwIO)
import Control.Exception (try)

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

newtype WrappedExc e =
  WrappedExc { unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show =
    mappend "WrappedExc: " . show . typeRep

instance Typeable e => Exception (WrappedExc e)

runStopAsExcFinal ::
  Typeable e =>
  Member (Final IO) r =>
  Sem (Stop e : r) a ->
  Sem r a
runStopAsExcFinal =
  interpretFinal \case
    Stop e ->
      pure (throwIO (WrappedExc e))
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
    pure $ either ((<$ s) . Left . unwrapExc) (fmap Right) <$> try m'
{-# INLINE stopToIOFinal #-}

-- |Stop if the argument is 'Left'.
stopEither ::
  Member (Stop err) r =>
  Either err a ->
  Sem r a
stopEither =
  either stop pure

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
