module Polysemy.Resume.Resumable where

import Polysemy.Internal (send, Sem(Sem), liftSem, runSem)
import Polysemy.Internal.Union (Union, Weaving(Weaving), decomp, hoist, injWeaving, weave)

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

interpretWithStop ::
  ∀ eff err a r0 f r res .
  Functor f =>
  (Either err (f a) -> Sem r res) ->
  (∀ r' . Member (Stop err) r' => InterpreterFor eff r') ->
  Weaving eff (Sem r0) a ->
  f () ->
  (∀ x . f (Sem r0 x) -> Sem r (f x)) ->
  (∀ x . f x -> Maybe x) ->
  Sem r res
interpretWithStop decideResult interpreter wav initialState distrib ins =
  decideResult =<< runStop (interpreter (liftSem wv1))
  where
    wv1 :: Union (eff : Stop err : r) (Sem (eff : Stop err : r)) (f a)
    wv1 =
      weave initialState (raise . raise . distrib) ins (injWeaving wav)
{-# INLINE interpretWithStop #-}

injectStop ::
  ∀ eff err handled r .
  (∀ f res (a :: *) . Functor f => f () -> (f (Either handled a) -> res) -> Either err (f a) -> Sem r res) ->
  (∀ r' . Member (Stop err) r' => InterpreterFor eff r') ->
  InterpreterFor (Resumable handled eff) r
injectStop decideResult interpreter sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist (injectStop decideResult interpreter) u) of
      Right (Weaving (Resumable wav) initialState distrib result ins) ->
        runSem (interpretWithStop (decideResult initialState result) interpreter wav initialState distrib ins) k
      Left g ->
        k g
{-# INLINE injectStop #-}

resumable ::
  ∀ eff err r .
  (∀ r' . Member (Stop err) r' => InterpreterFor eff r') ->
  InterpreterFor (Resumable err eff) r
resumable =
  injectStop (pure .:. distribEither)
{-# INLINE resumable #-}

resumePartial ::
  Functor f =>
  Member (Stop err) r =>
  (err -> Maybe handled) ->
  f () ->
  (f (Either handled a) -> res) ->
  Either err (f a) ->
  Sem r res
resumePartial handler initialState result = \case
  Right fa ->
    pure (result (Right <$> fa))
  Left (handler -> Just err) ->
    pure (result (Left err <$ initialState))
  Left err ->
    stop err
{-# INLINE resumePartial #-}

resumableFor ::
  ∀ eff err handled r .
  Member (Stop err) r =>
  (err -> Maybe handled) ->
  (∀ r' . Member (Stop err) r' => InterpreterFor eff r') ->
  InterpreterFor (Resumable handled eff) r
resumableFor handler =
  injectStop (resumePartial handler)
{-# INLINE resumableFor #-}

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
