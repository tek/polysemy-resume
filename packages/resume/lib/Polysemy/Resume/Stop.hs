module Polysemy.Resume.Stop where

import Control.Monad.Trans.Except (throwE)
import Polysemy.Internal (Sem(Sem))
import Polysemy.Internal.Union (Weaving(Weaving), decomp, weave)

import Polysemy.Resume.Data.Stop (Stop(Stop), stop)

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left _) = Nothing

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

errorStop ::
  Member (Stop err) r =>
  Sem (Error err : r) a ->
  Sem r a
errorStop sem =
  runError sem >>= \case
    Right a -> pure a
    Left err -> stop err
{-# INLINE errorStop #-}
