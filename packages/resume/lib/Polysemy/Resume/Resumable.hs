module Polysemy.Resume.Resumable where

import Polysemy.Internal (Sem(Sem), liftSem, raise, raiseUnder, runSem, send)
import Polysemy.Internal.Union (Weaving(Weaving), decomp, hoist, inj, injWeaving, weave)

import Polysemy.Error (Error(Throw), catchJust)
import Polysemy.Resume.Data.Resumable (Resumable(..))
import Polysemy.Resume.Data.Stop (Stop, stop)
import Polysemy.Resume.Stop (runStop, stopOnError)

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


-- |Convert a bare interpreter for @eff@, which (potentially) uses 'Stop' to signal errors, into an interpreter for
-- 'Resumable'.
--
-- >>> run $ resumable interpretStopper (interpretResumer mainProgram)
-- 237
resumable ::
  ∀ (eff :: Effect) (err :: *) (r :: EffectRow) .
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

-- |Convert an interpreter for @eff@ that uses 'Error' into one using 'Stop' and wrap it using 'resumable'.
resumableError ::
  ∀ eff err r .
  InterpreterFor eff (Error err : Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumableError interpreter =
  resumable (stopOnError . interpreter . raiseUnder)
{-# INLINE resumableError #-}

-- |Convert an interpreter for @eff@ that throws errors of type @err@ into a @Resumable@, but limiting the errors
-- handled by consumers to the type @handled@, which rethrowing 'Error's of type @unhandled@.
--
-- The function @canHandle@ determines how the errors are split.
--
-- @
-- newtype Blip =
--   Blip { unBlip :: Int }
--   deriving (Eq, Show)
--
-- bangOnly :: Boom -> Either Text Blip
-- bangOnly = \\case
--   Bang n -> Right (Blip n)
--   Boom msg -> Left msg
--
-- interpretResumerPartial ::
--   Member (Resumable Blip Stopper) r =>
--   InterpreterFor Resumer r
-- interpretResumerPartial =
--   interpret \\ MainProgram ->
--     resume (192 \<$ stopBang) \\ (Blip num) ->
--       pure (num * 3)
-- @
--
-- >>> runError (resumableFor bangOnly interpretStopper (interpretResumerPartial mainProgram))
-- Right 39
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

-- |Variant of 'resumableOr' that uses 'Maybe' and rethrows the original error.
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

-- |Reinterpreting variant of 'resumableFor'.
catchResumable ::
  ∀ eff handled err r .
  Members [eff, Error err] r =>
  (err -> Maybe handled) ->
  InterpreterFor (Resumable handled eff) r
catchResumable canHandle sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist (catchResumable canHandle) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> runSem resultFromEff k
        where
          resultFromEff =
            catchJust canHandle (fmap Right $ liftSem $ weave s wv ins (injWeaving e)) (pure . Left)
      Left g ->
        k g
{-# INLINE catchResumable #-}

-- |Interpret an effect @eff@ by wrapping it in @Resumable@ and @Stop@ and leaving the rest up to the user.
runAsResumable ::
  ∀ err eff r .
  Members [Resumable err eff, Stop err] r =>
  InterpreterFor eff r
runAsResumable sem =
  Sem \ k -> runSem sem \ u ->
    case decomp (hoist runAsResumable u) of
      Right wav ->
        runSem (either stop pure =<< send (Resumable wav)) k
      Left g ->
        k g
{-# INLINE runAsResumable #-}
