{-# options_ghc -Wno-redundant-constraints #-}
module Polysemy.Resume.Resumable where

import Polysemy (Final, RunH, interpretH, raise2Under)
import Polysemy.Error (Error (Throw), catchJust)
import Polysemy.Internal (Sem (Sem, runSem), liftSem, raise, raiseUnder, subsumeUsing, usingSem)
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Union (ElemOf (Here, There), StT, Weaving (Weaving), decomp, hoist, inj, weave)
import Polysemy.Interpretation (propagate)

import Polysemy.Resume.Data.Resumable (Resumable (..))
import Polysemy.Resume.Data.Stop (Stop, stop)
import Polysemy.Resume.Stop (StopExc, runStop, stopOnError, stopToIOFinal)

type InterpreterTrans' eff eff' r r' =
  ∀ a b .
  (Sem (eff' : r') a -> Sem r b) ->
  Sem (eff : r) a ->
  Sem r b

type InterpreterTrans eff eff' r =
  InterpreterTrans' eff eff' r r

distribEither ::
  Monad z =>
  Functor (StT t) =>
  Applicative (t z) =>
  (∀ x . t z x -> z (StT t x)) ->
  (StT t (Either err a) -> res) ->
  Either err (StT t a) ->
  z res
distribEither lower result = \case
  Right a -> pure (result (Right <$> a))
  Left err -> result <$> lower (pure (Left err))

-- |Convert a bare interpreter for @eff@, which (potentially) uses 'Stop' to signal errors, into an interpreter for
-- 'Resumable'.
-- /Beware/: This will display unsound behaviour if:
-- * the interpreter is wrapped with actions of another effect, as in:
--
--   @
--   interpretEff :: InterpreterFor Eff r
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
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) .
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumable interpreter (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (resumable interpreter) u) of
      Right (Weaving (Resumable e) trans lower result) ->
        distribEither lower result =<< runSem resultFromEff k
        where
          resultFromEff =
            runStop $ interpreter $ liftSem $ weave (trans (raise . raise)) lower (inj e)
      Left g ->
        k g
{-# inline resumable #-}

-- |Convenience combinator for turning an interpreter that doesn't use 'Stop' into a 'Resumable'.
raiseResumable ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) .
  InterpreterTrans (Resumable err eff) eff r
raiseResumable interpreter =
  interpreter . normalize . raiseUnder
  where
    normalize :: InterpreterFor (Resumable err eff) (eff : r)
    normalize (Sem m) =
      Sem \ k -> m \ u ->
        case decomp (hoist normalize u) of
          Right (Weaving (Resumable e) trans lower result) ->
            result . fmap Right <$> usingSem k (liftSem (weave (trans id) lower (inj e)))
          Left g ->
            k g
    {-# inline normalize #-}
{-# inline raiseResumable #-}

-- |Like 'resumable', but use exceptions instead of 'ExceptT'.
resumableIO ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) .
  Exception (StopExc err) =>
  Member (Final IO) r =>
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumableIO interpreter (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (resumable interpreter) u) of
      Right (Weaving (Resumable e) trans lower result) ->
        distribEither lower result =<< runSem resultFromEff k
        where
          resultFromEff =
            stopToIOFinal $ interpreter $ liftSem $ weave (trans (raise . raise)) lower (inj e)
      Left g ->
        k g
{-# inline resumableIO #-}

-- |Like 'interpretResumable', but for higher-order effects.
interpretResumableH ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) (aa :: Type) .
  (∀ r0 t x . Traversable t => eff (Sem r0) x -> Sem (RunH (Sem r0) t (Resumable err eff) r : Stop err : r) x) ->
  Sem (Resumable err eff : r) aa ->
  Sem r aa
interpretResumableH handler =
  interpretH \case
    Resumable e ->
      runStop (subsumeUsing (There Here) (raise2Under (handler e)))
{-# inline interpretResumableH #-}

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
  ∀ (err :: Type) (eff :: Effect) r .
  FirstOrder eff "interpretResumable" =>
  (∀ x r0 . eff (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Resumable err eff) r
interpretResumable handler =
  interpretResumableH \ e -> raise (handler e)
{-# inline interpretResumable #-}

-- |Convert an interpreter for @eff@ that uses 'Error' into one using 'Stop' and wrap it using 'resumable'.
resumableError ::
  ∀ (err :: Type) (eff :: Effect) r .
  InterpreterFor eff (Error err : Stop err : r) ->
  InterpreterFor (Resumable err eff) r
resumableError interpreter =
  resumable (stopOnError . interpreter . raiseUnder)
{-# inline resumableError #-}

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
  ∀ (err :: Type) (eff :: Effect) unhandled handled r .
  Member (Error unhandled) r =>
  (err -> Either unhandled handled) ->
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable handled eff) r
resumableOr canHandle interpreter (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (resumableOr canHandle interpreter) u) of
      Right (Weaving (Resumable e) trans lower result) ->
        distribEither lower result =<< (tryHandle =<< runSem resultFromEff k)
        where
          tryHandle = \case
            Left err ->
              either (k . inj . Throw) (pure . Left) (canHandle err)
            Right a ->
              pure (Right a)
          resultFromEff =
            runStop $ interpreter $ liftSem $ weave (trans (raise . raise)) lower (inj e)
      Left g ->
        k g
{-# inline resumableOr #-}

-- |Variant of 'resumableOr' that uses 'Maybe' and rethrows the original error.
resumableFor ::
  ∀ (err :: Type) (eff :: Effect) handled r .
  Member (Error err) r =>
  (err -> Maybe handled) ->
  InterpreterFor eff (Stop err : r) ->
  InterpreterFor (Resumable handled eff) r
resumableFor canHandle =
  resumableOr canHandle'
  where
    canHandle' err =
      maybeToRight err (canHandle err)
{-# inline resumableFor #-}

-- |Reinterpreting variant of 'resumableFor'.
catchResumable ::
  ∀ (err :: Type) (eff :: Effect) handled r .
  Members [eff, Error err] r =>
  (err -> Maybe handled) ->
  InterpreterFor (Resumable handled eff) r
catchResumable canHandle (Sem m) =
  Sem \ k -> m \ u ->
    case decomp (hoist (catchResumable canHandle) u) of
      Right (Weaving (Resumable e) trans lower result) ->
        distribEither lower result =<< runSem resultFromEff k
        where
          resultFromEff =
            catchJust canHandle (fmap Right $ liftSem $ weave (trans id) lower (inj e)) (pure . Left)
      Left g ->
        k g
{-# inline catchResumable #-}

-- |Interpret an effect @eff@ by wrapping it in @Resumable@ and @Stop@ and leaving the rest up to the user.
runAsResumable ::
  ∀ (err :: Type) (eff :: Effect) r .
  Members [Resumable err eff, Stop err] r =>
  InterpreterFor eff r
runAsResumable =
  interpretH \ e ->
    either (stop @err) pure =<< propagate (Resumable e)
{-# inline runAsResumable #-}
