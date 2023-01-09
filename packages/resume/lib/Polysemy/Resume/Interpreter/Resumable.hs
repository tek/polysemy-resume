{-# options_haddock prune #-}
{-# options_ghc -Wno-redundant-constraints #-}

-- | Description: Interpreters for 'Resumable'.
module Polysemy.Resume.Interpreter.Resumable where

import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Union (ElemOf, membership)
import Polysemy.Membership (exposeUsing)
import Polysemy.Meta (interpretMeta, runExposeMeta)
import Polysemy.Newtype (coerceEff)
import Polysemy.Opaque (Opaque, fromOpaque, toOpaqueAt)

import Polysemy.Resume.Effect.Resumable (Resumable (..), ResumableMeta (ResumableMeta))
import Polysemy.Resume.Effect.RunStop (RunStop)
import Polysemy.Resume.Effect.Stop (Stop)
import Polysemy.Resume.Interpreter.Stop (runStop)
import Polysemy.HigherOrder (restoreH)

type ResumableInterpreter err eff r =
  ∀ q x . Sem (eff : Opaque q : r) x -> Sem (Opaque q : r) (Either err x)

type ResumableHandler err eff r =
  ∀ q . EffHandlerH eff (Stop err : Opaque q : r)

runResumableWith ::
  ∀ err eff r .
  ResumableInterpreter err eff r ->
  InterpreterFor (Resumable err eff) r
runResumableWith int =
  coerceEff >>>
  interpretMeta @(ResumableMeta eff err) \case
    ResumableMeta m ->
      runExposeMeta
        (toOpaqueAt @'[_]
         >>> int
         >>> coerceEff
         >>> fromOpaque
         ) m >>= \case
        Left e -> pure (Left e)
        Right ta -> Right <$> restoreH ta

-- | Create an interpreter for @'Resumable' err eff@ from an interpreter for @eff@.
--
-- /Note/: Any setup code hidden in the interpreter's closure will be executed on each call to 'resume' or similar.
runResumable ::
  ∀ err eff r .
  Member RunStop r =>
  (∀ q . InterpreterFor eff (Stop err : Opaque q : r)) ->
  InterpreterFor (Resumable err eff) r
runResumable int =
  runResumableWith (runStop . int . raiseUnder)

-- | Like 'interpretResumable', but for higher-order effects.
interpretResumableH ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) .
  Member RunStop r =>
  -- | This handler has @'Stop' err@ in its stack, allowing the interpreter to absorb its errors.
  ResumableHandler err eff r ->
  InterpreterFor (Resumable err eff) r
interpretResumableH handler =
  runResumableWith (runStop . reinterpretH \ eff -> handler eff)

-- | Create an interpreter for @'Resumable' err eff@ by supplying a handler function for @eff@, analogous to
-- 'Polysemy.interpret'.
-- If the handler throws errors with 'Stop', they will be absorbed into 'Resumable', to be caught by
-- 'Polysemy.Resume.resume' in upstream code.
--
-- @
-- interpretStopperResumable ::
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
  ∀ err eff r .
  Member RunStop r =>
  FirstOrder eff "interpretResumable" =>
  (∀ z q x . eff z x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Resumable err eff) r
interpretResumable handler =
  runResumableWith (runStop . reinterpretH (raise . handler))

-- | Interceptor variant of 'interpretResumableH'.
interceptResumableUsingH ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) (a :: Type) .
  Member RunStop r =>
  ElemOf (Resumable err eff) r ->
  ResumableHandler err eff r ->
  Sem r a ->
  Sem r a
interceptResumableUsingH proof handler =
  interpretResumableH handler . exposeUsing proof

-- | Interceptor variant of 'interpretResumable'.
interceptResumableUsing ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) (a :: Type) .
  Member RunStop r =>
  FirstOrder eff "interceptResumableUsing" =>
  ElemOf (Resumable err eff) r ->
  (∀ z x . eff z x -> Sem (Stop err : r) x) ->
  Sem r a ->
  Sem r a
interceptResumableUsing proof f =
  interceptResumableUsingH proof \ e -> raise (raiseUnder (f e))

-- | Interceptor variant of 'interpretResumableH'.
interceptResumableH ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) (a :: Type) .
  Member RunStop r =>
  Member (Resumable err eff) r =>
  ResumableHandler err eff r ->
  Sem r a ->
  Sem r a
interceptResumableH =
  interceptResumableUsingH membership

-- | Interceptor variant of 'interpretResumable'.
interceptResumable ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) (a :: Type) .
  Members [Resumable err eff, RunStop] r =>
  FirstOrder eff "interceptResumable" =>
  (∀ z x . eff z x -> Sem (Stop err : r) x) ->
  Sem r a ->
  Sem r a
interceptResumable f =
  interceptResumableH \ e -> raise (raiseUnder (f e))

-- | Higher-order variant of 'interpretResumerPartial'.
interpretResumablePartialH ::
  ∀ (err :: Type) (eff :: Effect) unhandled handled r .
  Member RunStop r =>
  Member (Error unhandled) r =>
  (err -> Either unhandled handled) ->
  ResumableHandler err eff r ->
  InterpreterFor (Resumable handled eff) r
interpretResumablePartialH canHandle handler =
  runResumableWith $
    let
      check = \case
        Right handled -> pure (Left handled)
        Left unhandled -> throw unhandled
    in either (check . canHandle) (pure . Right) <=< runStop . reinterpretH \ eff -> handler eff

-- | Convert an interpreter for @eff@ that throws errors of type @err@ into a @Resumable@, but limiting the errors
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
-- >>> runError (interpretResumablePartial bangOnly handleStopper (interpretResumerPartial mainProgram))
-- Right 39
interpretResumablePartial ::
  ∀ (err :: Type) (eff :: Effect) unhandled handled r .
  Member RunStop r =>
  Member (Error unhandled) r =>
  (err -> Either unhandled handled) ->
  (∀ z x . eff z x -> Sem (Stop err : r) x) ->
  InterpreterFor (Resumable handled eff) r
interpretResumablePartial canHandle handler =
  interpretResumablePartialH canHandle \ e -> raise (raiseUnder (handler e))

-- | Variant of 'interpretResumablePartialH' that uses 'Maybe' and rethrows the original error.
interpretResumableForH ::
  ∀ (err :: Type) (eff :: Effect) handled r .
  Member RunStop r =>
  Member (Error err) r =>
  (err -> Maybe handled) ->
  ResumableHandler err eff r ->
  InterpreterFor (Resumable handled eff) r
interpretResumableForH canHandle =
  interpretResumablePartialH canHandle'
  where
    canHandle' err = maybeToRight err (canHandle err)

-- | Variant of 'interpretResumablePartial' that uses 'Maybe' and rethrows the original error.
interpretResumableFor ::
  ∀ (err :: Type) (eff :: Effect) handled r .
  Member RunStop r =>
  Member (Error err) r =>
  (err -> Maybe handled) ->
  (∀ z x . eff z x -> Sem (Stop err : r) x) ->
  InterpreterFor (Resumable handled eff) r
interpretResumableFor canHandle handler =
  interpretResumableForH canHandle \ e -> raise (raiseUnder (handler e))

-- | Convenience combinator for turning an interpreter that doesn't use 'Stop' into a 'Resumable'.
raiseResumable ::
  ∀ (err :: Type) (eff :: Effect) (r :: EffectRow) (a :: Type) .
  (∀ q x . Sem (eff : Opaque q : r) x -> Sem (Opaque q : r) x) ->
  Sem (Resumable err eff : r) a ->
  Sem r a
raiseResumable interpreter =
  runResumableWith (fmap Right . interpreter)
