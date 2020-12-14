module Polysemy.Resume.Resumable where

import Polysemy (Final, Tactical)
import Polysemy.Error (Error(Throw), catchJust)
import Polysemy.Internal (Sem(Sem, runSem), liftSem, raise, raiseUnder, send, usingSem)
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Tactics (liftT, runTactics)
import Polysemy.Internal.Union (Weaving(Weaving), decomp, hoist, inj, injWeaving, weave)

import Polysemy.Resume.Data.Resumable (Resumable(..))
import Polysemy.Resume.Data.Stop (Stop, stop)
import Polysemy.Resume.Stop (runStop, stopOnError, stopToIOFinal)

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
-- /Beware/: This will display unsound behaviour if:
-- * the interpreter is wrapped with actions of another effect, as in:
--
--   @
--   interpretEffResumable :: InterpreterFor Eff r
--   ...
--
--   interpretEffResumable :: InterpreterFor (Resumable Text Eff) r
--   interpretEffResumable sem =
--   resumable (interpretEff (sem `finally` releaseResources))
--   @
--
--   In this case, @releaseResources@ will be called after /every/ use of @Eff@ in @sem@, not after the entire thunk.
--
-- * the interpreter of a higher-order effect uses a different interpreter after using @runT@/@bindT@.
--   In this case, it will use the original interpreter instead.
--
-- If your use case matches one of these conditions, you'll need to use 'interpretResumable'.
--
-- >>> run $ resumable interpretStopper (interpretResumer mainProgram)
-- 237
resumable ::
  ∀ (err :: *) (eff :: Effect) (r :: EffectRow) .
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumable interpreter (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (resumable interpreter) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> runSem resultFromEff k
        where
          resultFromEff =
            runStop $ interpreter $ liftSem $ weave s (raise . raise . wv) ins (injWeaving e)
      Left g ->
        k g
{-# INLINE resumable #-}

-- |Convenience combinator for turning an interpreter that doesn't use 'Stop' into a 'Resumable'.
raiseResumable ::
  ∀ (err :: *) (eff :: Effect) (r :: EffectRow) .
  InterpreterFor eff r ->
  InterpreterFor (Resumable err eff) r
raiseResumable interpreter (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (raiseResumable interpreter) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> runSem resultFromEff k
        where
          resultFromEff =
            fmap Right $ interpreter $ liftSem $ weave s (raise . wv) ins (injWeaving e)
      Left g ->
        k g
{-# INLINE raiseResumable #-}

-- |Like 'resumable', but use exceptions instead of 'ExceptT'.
resumableIO ::
  ∀ (err :: *) (eff :: Effect) (r :: EffectRow) .
  Typeable err =>
  Member (Final IO) r =>
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumableIO interpreter (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (resumable interpreter) u) of
      Right (Weaving (Resumable e) s wv ex ins) ->
        distribEither s ex <$> runSem resultFromEff k
        where
          resultFromEff =
            stopToIOFinal $ interpreter $ liftSem $ weave s (raise . raise . wv) ins (injWeaving e)
      Left g ->
        k g
{-# INLINE resumableIO #-}

-- |Like 'interpretResumable', but for higher-order effects.
interpretResumableH ::
  ∀ (err :: *) (eff :: Effect) (r :: EffectRow) .
  -- |This handler function has @'Stop' err@ in its stack, allowing it to absorb errors.
  (∀ x r0 . eff (Sem r0) x -> Tactical (Resumable err eff) (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Resumable err eff) r
interpretResumableH handler (Sem m) =
  Sem \ k -> m \ u ->
    case decomp u of
      Left there ->
        k (hoist (interpretResumableH handler) there)
      Right (Weaving (Resumable (Weaving e s dist ex ins)) sOuter distOuter exOuter insOuter) ->
        usingSem k (exFinal <$> runStop tac)
        where
          tac =
            runTactics
            (Compose (s <$ sOuter))
            (raiseUnder . fmap Compose . distOuter . fmap dist . getCompose)
            (join . fmap ins . insOuter . getCompose)
            (raise . interpretResumableH handler . fmap Compose . distOuter . fmap dist . getCompose)
            (handler e)
          exFinal = exOuter . \case
            Right (getCompose -> a) -> Right . ex <$> a
            Left err -> Left err <$ sOuter
{-# INLINE interpretResumableH #-}

-- |Create an interpreter for @'Resumable' err eff@ by supplying a handler function for @eff@, analogous to
-- 'Polysemy.interpret'.
-- If the handler throws errors with 'Stop', they will be absorbed into 'Resumable', to be caught by
-- 'Polysemy.Resume.resume' in a downstream interpreter.
--
-- @
-- interpretStopperResumable ::
--   Member (Stop Boom) r =>
--   InterpreterFor Stopper r
-- interpretStopperResumable =
--   interpretResumable \\case
--     StopBang -> stop (Bang 13)
--     StopBoom -> stop (Boom "ouch")
-- @
--
-- >>> run $ interpretStopperResumable (interpretResumer mainProgram)
-- 237
interpretResumable ::
  ∀ (err :: *) (eff :: Effect) r .
  FirstOrder eff "interpretResumable" =>
  (∀ x r0 . eff (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Resumable err eff) r
interpretResumable handler =
  interpretResumableH (liftT . handler)
{-# INLINE interpretResumable #-}

-- |Convert an interpreter for @eff@ that uses 'Error' into one using 'Stop' and wrap it using 'resumable'.
resumableError ::
  ∀ (err :: *) (eff :: Effect) r .
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
  ∀ (err :: *) (eff :: Effect) unhandled handled r .
  Member (Error unhandled) r =>
  (err -> Either unhandled handled) ->
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable handled eff) r
resumableOr canHandle interpreter (Sem m) =
  Sem \ k -> m \ u ->
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
  ∀ (err :: *) (eff :: Effect) handled r .
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
  ∀ (err :: *) (eff :: Effect) handled r .
  Members [eff, Error err] r =>
  (err -> Maybe handled) ->
  InterpreterFor (Resumable handled eff) r
catchResumable canHandle (Sem m) =
  Sem \ k -> m \ u ->
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
  ∀ (err :: *) (eff :: Effect) r .
  Members [Resumable err eff, Stop err] r =>
  InterpreterFor eff r
runAsResumable (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (runAsResumable @err @eff) u) of
      Right wav ->
        runSem (either (stop @err) pure =<< send (Resumable wav)) k
      Left g ->
        k g
{-# INLINE runAsResumable #-}
