module Polysemy.Resume.Stop where

import Control.Monad.Trans.Except (throwE)
import Polysemy.Error (runError, throw)
import Polysemy.Internal (Sem(Sem))
import Polysemy.Internal.Union (Weaving(Weaving), decomp, weave)

import Polysemy.Resume.Data.Stop (Stop(Stop), stop)

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left _) = Nothing

-- |Equivalent of 'runError'.
runStop ::
  Sem (Stop e : r) a ->
  Sem r (Either e a)
runStop (Sem m) =
  Sem \ k -> runExceptT $ m \ u ->
    case decomp u of
      Left x ->
        ExceptT $ k $ weave (Right ()) (either (pure . Left) runStop) hush x
      Right (Weaving (Stop e) _ _ _ _) ->
        throwE e
{-# INLINE runStop #-}

-- |Convert a program using regular 'Error's to one using 'Stop'.
stopOnError ::
  Member (Stop err) r =>
  Sem (Error err : r) a ->
  Sem r a
stopOnError sem =
  runError sem >>= \case
    Right a -> pure a
    Left err -> stop err
{-# INLINE stopOnError #-}

-- |Convert a program using 'Stop' to one using 'Error'.
stopToError ::
  Member (Error err) r =>
  Sem (Stop err : r) a ->
  Sem r a
stopToError sem =
  runStop sem >>= \case
    Right a -> pure a
    Left err -> throw err
{-# INLINE stopToError #-}

-- |Stop if the argument is 'Left'.
stopEither ::
  Member (Stop err) r =>
  Either err a ->
  Sem r a
stopEither =
  either stop pure
