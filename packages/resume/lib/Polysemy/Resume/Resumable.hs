module Polysemy.Resume.Resumable where

import Polysemy.Internal (Sem(Sem), liftSem, runSem, send)
import Polysemy.Internal.Union (Weaving(Weaving), decomp, hoist, inj, injWeaving, weave)

import Polysemy.Error (catchJust, Error(Throw))
import Polysemy.Resume.Data.Resumable (Resumable(..))
import Polysemy.Resume.Data.Stop (Stop, stop)
import Polysemy.Resume.Stop (runStop)

distribEither ::
  Functor f =>
  f () ->
  (f (Either err a) -> res) ->
  Either err (f a) ->
  res
distribEither initialState result =
  result . \case
    Right fa -> Right <$> fa
    Left err -> Left err <$ initialState
{-# INLINE distribEither #-}

resumable ::
  ∀ eff err r .
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumable interpreter sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist (resumable interpreter) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> runSem resultFromEff k
        where
          resultFromEff =
            runStop $ interpreter $ liftSem $ weave s (raise . raise . wv) ins (injWeaving e)
      Left g ->
        k g
{-# INLINE resumable #-}

resumableOr ::
  ∀ eff err unhandled handled r .
  Member (Error unhandled) r =>
  (err -> Either unhandled handled) ->
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable handled eff) r
resumableOr canHandle interpreter sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist (resumableOr canHandle interpreter) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> (tryHandle =<< runSem resultFromEff k)
        where
          tryHandle = \case
            Left err ->
              either (k . inj . Throw) (pure . Left) (canHandle err)
            Right a ->
              pure (Right a)
          resultFromEff =
            runStop $ interpreter $ liftSem $ weave s (raise . raise . wv) ins (injWeaving e)
      Left g ->
        k g
{-# INLINE resumableOr #-}

resumableFor ::
  ∀ eff err handled r .
  Member (Error err) r =>
  (err -> Maybe handled) ->
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable handled eff) r
resumableFor canHandle =
  resumableOr canHandle'
  where
    canHandle' err =
      maybeToRight err (canHandle err)
{-# INLINE resumableFor #-}

runResumable ::
  ∀ eff handled err r .
  Members [eff, Error err] r =>
  (err -> Maybe handled) ->
  InterpreterFor (Resumable handled eff) r
runResumable canHandle sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist (runResumable canHandle) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> runSem resultFromEff k
        where
          resultFromEff =
            catchJust canHandle (fmap Right $ liftSem $ weave s wv ins (injWeaving e)) (pure . Left)
      Left g ->
        k g
{-# INLINE runResumable #-}

stopResumable ::
  ∀ err eff r .
  Members [Resumable err eff, Stop err] r =>
  InterpreterFor eff r
stopResumable sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist stopResumable u) of
      Right wav ->
        runSem (either stop pure =<< send (Resumable wav)) k
      Left g ->
        k g
{-# INLINE stopResumable #-}

resume ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  Sem (eff : r) a ->
  (err -> Sem r a) ->
  Sem r a
resume sem handler =
  either handler pure =<< runStop (stopResumable (raiseUnder sem))
{-# INLINE resume #-}
