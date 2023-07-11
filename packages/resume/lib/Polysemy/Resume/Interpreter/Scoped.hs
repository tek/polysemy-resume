{-# options_haddock prune #-}

-- |Description: Interpreters for 'Scoped' in combination with 'Resumable'
--
-- @since 0.6.0.0
module Polysemy.Resume.Interpreter.Scoped where

import GHC.Err (errorWithoutStackTrace)
import Polysemy.Internal (liftSem, restack)
import Polysemy.Internal.Combinators (interpretWeaving)
import Polysemy.Internal.Scoped (OuterRun (OuterRun), Scoped (InScope, Run))
import Polysemy.Internal.Sing (KnownList (singList))
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Internal.Union (Weaving (Weaving), injWeaving, injectMembership, weave)
import Polysemy.Opaque (Opaque (Opaque))

import Polysemy.Resume.Effect.Resumable (Resumable (Resumable), type (!!))
import Polysemy.Resume.Effect.Stop (Stop)
import Polysemy.Resume.Interpreter.Resumable (interpretResumableH)
import Polysemy.Resume.Interpreter.Stop (runStop)

exResumable ::
  Functor f =>
  f () ->
  (f (Either err a) -> x) ->
  (f' a' -> a) ->
  Sem r (Either err (f (f' a'))) ->
  Sem r x
exResumable s ex ex' =
  fmap $ ex . \case
    Right a -> Right . ex' <$> a
    Left err -> Left err <$ s
{-# inline exResumable #-}

runScopedResumable ::
  ∀ param effect err r .
  (∀ q. param -> InterpreterFor effect (Stop err : Opaque q : r)) ->
  InterpreterFor (Scoped param effect !! err) r
runScopedResumable h =
  interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' _)) s wv ex _) -> case effect of
    Run w _ -> errorWithoutStackTrace $ "top level run with depth " ++ show w
    InScope param main ->
      wv (wv' (main 0 <$ s') <$ s)
        & raiseUnder2
        & go 0
        & raiseUnder
        & h param
        & runStop
        & interpretH (\(Opaque (OuterRun w _)) ->
            errorWithoutStackTrace $ "unhandled OuterRun with depth " ++ show w)
        & exResumable s ex ex'
  where
    go' ::
      Word ->
      InterpreterFor (Opaque (OuterRun effect)) (effect : Opaque (OuterRun effect) : r)
    go' depth =
      interpretWeaving \ (Weaving sr@(Opaque (OuterRun w act)) s wv ex ins) ->
        if w == depth then
          liftSem $ injWeaving $ Weaving act s (go' depth . wv) ex ins
        else
          liftSem $ injWeaving $ Weaving sr s (go' depth . wv) ex ins
    go ::
      Word ->
      InterpreterFor (Scoped param effect !! err) (effect : Opaque (OuterRun effect) : r)
    go depth =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        Run w act
          | w == depth ->
            ex . fmap Right <$> liftSem (weave s (go depth . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))
          | otherwise ->
            ex . fmap Right <$> liftSem (weave s (go depth . wv) ins (injWeaving (Weaving (Opaque (OuterRun w act)) s' wv' ex' ins')))
        InScope param main -> do
          let !depth' = depth + 1
          wv (wv' (main depth' <$ s') <$ s)
            & go depth'
            & raiseUnder
            & h param
            & runStop
            & raiseUnder2
            & go' depth
            & exResumable s ex ex'
{-# inline runScopedResumable #-}

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ param resource effect err r .
  (∀ q x . param -> (resource -> Sem (Stop err : Opaque q : r) x) -> Sem (Stop err : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableH withResource scopedHandler =
  runScopedResumable \ param sem ->
    withResource param \ r -> interpretH (scopedHandler r) sem

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ param resource effect err r .
  (∀ q x . param -> (resource -> Sem (Stop err : Opaque q : r) x) -> Sem (Stop err : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ param resource effect err r .
  (∀ q . param -> Sem (Stop err : Opaque q : r) resource) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumable_ resource =
  interpretScopedResumable \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra param resource effect err r .
  KnownList extra =>
  (∀ q x . param -> (resource -> Sem (extra ++ Stop err : Opaque q : r) x) -> Sem (Stop err : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (extra ++ [Stop err, Opaque q] ++ r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Stop err : Opaque q : r) x) ->
    withResource param \ resource ->
      interpretH (scopedHandler @q resource) $
      restack (injectMembership (singList @'[effect]) (singList @extra)) $
      sem

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWith ::
  ∀ extra param resource effect err r .
  KnownList extra =>
  (∀ q x . param -> (resource -> Sem (extra ++ Stop err : Opaque q : r) x) -> Sem (Stop err : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (extra ++ Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWith withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Stop err : Opaque q : r) x) ->
    withResource param \ resource ->
      sem
      & restack (injectMembership (singList @'[effect]) (singList @extra))
      & interpretH \ e -> liftT (scopedHandler @q resource e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra param effect err r .
  KnownList extra =>
  (∀ q x . param -> Sem (extra ++ Stop err : Opaque q : r) x -> Sem (Stop err : Opaque q : r) x) ->
  (∀ q r0 x . effect (Sem r0) x -> Sem (extra ++ Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Stop err : Opaque q : r) x) ->
    extra @q param $
      sem
      & restack (injectMembership (singList @'[effect]) (singList @extra))
      & interpretH \ e -> liftT (scopedHandler @q e)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ param resource effect err r .
  (∀ q x . param -> (resource -> Sem (Opaque q : r) x) -> Sem (Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! err) (Sem r0) (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedH withResource scopedHandler =
  runScopedNew \ param sem ->
    withResource param \ r ->
      interpretResumableH (scopedHandler r) sem

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ param resource effect err r .
  (∀ q x . param -> (resource -> Sem (Opaque q : r) x) -> Sem (Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScoped withResource scopedHandler =
  interpretResumableScopedH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumableScoped_ ::
  ∀ param resource effect err r .
  (param -> Sem r resource) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScoped_ resource =
  interpretResumableScoped \ p use -> use =<< raise (resource p)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWithH ::
  ∀ extra param resource effect err r .
  KnownList extra =>
  (∀ q x . param -> (resource -> Sem (extra ++ Opaque q : r) x) -> Sem (Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! err) (Sem r0) (Stop err : (extra ++ Opaque q : r)) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWithH withResource scopedHandler =
  runScopedNew \ param (sem :: Sem (effect !! err : Opaque q : r) x) ->
    withResource param \ resource ->
      sem
        & restack (injectMembership (singList @'[effect !! err]) (singList @extra))
        & interpretResumableH (scopedHandler @q resource)

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWith ::
  ∀ extra param resource effect err r .
  KnownList extra =>
  (∀ q x . param -> (resource -> Sem (extra ++ Opaque q : r) x) -> Sem (Opaque q : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : (extra ++ r)) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWith withResource scopedHandler =
  runScopedNew \ param (sem :: Sem (effect !! err : Opaque q : r) x) ->
    withResource param \resource ->
      sem
        & restack (injectMembership (singList @'[effect !! err]) (singList @extra))
        & interpretResumableH \ e ->
          liftT $
          restack (injectMembership @r (singList @(Stop err : extra)) (singList @'[Opaque q])) (scopedHandler resource e)

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumableScopedWith_ ::
  ∀ extra param effect err r .
  KnownList extra =>
  (∀ q x . param -> Sem (extra ++ Opaque q : r) x -> Sem (Opaque q : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : (extra ++ r)) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWith_ extra scopedHandler =
  interpretResumableScopedWith @extra @param @() @effect @err @r (\ p f -> extra p (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRH ::
  ∀ param resource effect eo ei r .
  (∀ q x . param -> (resource -> Sem (Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! ei) (Sem r0) (Stop ei : Stop eo : Opaque q : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRH withResource scopedHandler =
  runScopedResumable \ param sem ->
    withResource param \ r ->
      interpretResumableH (scopedHandler r) sem

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedR ::
  ∀ param resource effect eo ei r .
  (∀ q x . param -> (resource -> Sem (Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : Stop eo : Opaque q : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedR withResource scopedHandler =
  interpretScopedRH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant:
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedR_ ::
  ∀ param resource effect eo ei r .
  (param -> Sem (Stop eo : r) resource) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : Stop eo : Opaque q : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedR_ resource =
  interpretScopedR \ p use -> use =<< raiseUnder (resource p)

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWithH ::
  ∀ extra param resource effect eo ei r .
  KnownList extra =>
  (∀ q x . param -> (resource -> Sem (extra ++ Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! ei) (Sem r0) (Stop ei : (extra ++ Stop eo : Opaque q : r)) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWithH withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect !! ei : Stop eo : Opaque q : r) x) ->
    withResource param \ resource ->
      sem
        & restack (injectMembership (singList @'[effect !! ei]) (singList @extra))
        & interpretResumableH (scopedHandler @q resource)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWith ::
  ∀ extra param resource effect eo ei r .
  KnownList extra =>
  (∀ q x . param -> (resource -> Sem (extra ++ Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
  (∀ q r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : (extra ++ Stop eo : Opaque q : r)) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWith withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect !! ei : Stop eo : Opaque q : r) x) ->
    withResource param \ resource ->
      sem
      & restack (injectMembership (singList @'[effect !! ei]) (singList @extra))
      & interpretResumableH \ e ->
        liftT (scopedHandler @q resource e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedRWith_ ::
  ∀ extra param effect eo ei r .
  KnownList extra =>
  (∀ q x . param -> Sem (extra ++ Stop eo : Opaque q : r) x -> Sem (Stop eo : Opaque q : r) x) ->
  (∀ q r0 x . effect (Sem r0) x -> Sem (Stop ei : (extra ++ Stop eo : Opaque q : r)) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWith_ extra scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect !! ei : Stop eo : Opaque q : r) x) ->
    extra param do
      sem
        & restack (injectMembership (singList @'[effect !! ei]) (singList @extra))
        & interpretResumableH \ e ->
          liftT (scopedHandler @q e)
