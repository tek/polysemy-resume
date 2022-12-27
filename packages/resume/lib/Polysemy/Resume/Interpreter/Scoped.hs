{-# options_haddock prune #-}

-- |Description: Interpreters for 'Scoped' in combination with 'Resumable'
module Polysemy.Resume.Interpreter.Scoped where

import GHC.Err (errorWithoutStackTrace)
import Polysemy.Internal (liftSem, restack)
import Polysemy.Internal.Combinators (interpretWeaving)
import Polysemy.Internal.Scoped (Scoped (InScope, Run))
import Polysemy.Internal.Sing (KnownList (singList))
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Internal.Union (Weaving (Weaving), extendMembershipLeft, injWeaving, injectMembership, weave)

import Polysemy.Resume.Effect.Resumable (Resumable (Resumable), type (!!))
import Polysemy.Resume.Effect.Stop (Stop)
import Polysemy.Resume.Resumable (interpretResumableH)
import Polysemy.Resume.Stop (runStop)

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

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretH (scopedHandler resource) (raiseUnder (go $ raiseUnder $ wv ((wv' (main <$ s')) <$ s)))
    _ ->
      errorWithoutStackTrace "top level run"
  where
    go :: InterpreterFor (Scoped param effect !! err) (effect : r)
    go =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        Run act ->
          ex . fmap Right <$> liftSem (weave s (go . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))
        InScope param main -> do
          exResumable s ex ex' $ raise $ runStop $ withResource param \ resource ->
            interpretH (scopedHandler resource) (raiseUnder (go $ wv (wv' (main <$ s') <$ s)))

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem (Stop err : r) x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ param resource effect err r .
  (param -> Sem (Stop err : r) resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumable_ resource =
  interpretScopedResumable \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra param resource effect err r r1 extraerr .
  extraerr ~ (extra ++ '[Stop err]) =>
  r1 ~ (extraerr ++ r) =>
  KnownList (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param effect !! err])
             (singList @(effect : extraerr))) $ wv (wv' (main <$ s') <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param effect !! err) (effect : r1)
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        InScope param main ->
          restack
            (extendMembershipLeft (singList @(effect : extraerr))) $
            exResumable s ex ex' $ runStop $ withResource param \ resource ->
              interpretH (scopedHandler resource) (inScope $ wv (wv' (main <$ s') <$ s))
        Run act ->
          ex . fmap Right <$> liftSem (weave s (inScope . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  KnownList (extra ++ '[Stop err]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop err : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWith withResource scopedHandler =
  interpretScopedResumableWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ ((extra ++ '[Stop err]) ++ r) =>
  KnownList (extra ++ '[Stop err]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop err : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  interpretScopedResumableWith @extra (\ p f -> extra p (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! err) (Sem r0) (Stop err : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedH withResource scopedHandler =
  interpretWeaving $ \(Weaving effect s wv ex _) -> case effect of
    Run _ -> errorWithoutStackTrace "top level run"
    InScope param main -> withResource param \ resource ->
      ex <$> interpretResumableH (scopedHandler resource) (inScope $ raiseUnder $ wv (main <$ s))
  where
    inScope :: InterpreterFor (Scoped param (effect !! err)) (effect !! err : r)
    inScope =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run act -> liftSem $ injWeaving $ Weaving act s (inScope . wv) ex ins
        InScope param main -> raise $ withResource param \ resource ->
          ex <$> interpretResumableH (scopedHandler resource) (inScope $ wv (main <$ s))

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ param resource effect err r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
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
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScoped_ resource =
  interpretResumableScoped \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWithH ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! err) (Sem r0) (Stop err : r1) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource ->
        interpretResumableH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param (effect !! err)])
             (singList @(effect !! err : extra))) $ wv (main <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param (effect !! err)) (effect !! err : r1)
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          restack
            (extendMembershipLeft (singList @(effect !! err : extra)))
            (ex <$> withResource param \resource ->
                interpretResumableH (scopedHandler resource) $ inScope $ wv (main <$ s))
        Weaving (Run act) s wv ex ins ->
          liftSem $ injWeaving $ Weaving act s (inScope . wv) ex ins

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedWith ::
  ∀ extra param resource effect err r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWith withResource scopedHandler =
  interpretResumableScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - No resource is used and the allocator is a plain interpreter.
interpretResumableScopedWith_ ::
  ∀ extra param effect err r r1 .
  r1 ~ (extra ++ r) =>
  KnownList extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop err : r1) x) ->
  InterpreterFor (Scoped param (effect !! err)) r
interpretResumableScopedWith_ extra scopedHandler =
  interpretResumableScopedWith @extra @param @() @effect @err @r @r1 (\ p f -> extra p (f ())) (const scopedHandler)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRH ::
  ∀ param resource effect eo ei r .
  (∀ x . param -> (resource -> Sem (Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! ei) (Sem r0) (Stop ei : Stop eo : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretResumableH (scopedHandler resource) (raiseUnder (inScope $ raiseUnder $ wv (wv' (main <$ s') <$ s)))
    _ ->
      errorWithoutStackTrace "top level run"
  where
    inScope :: InterpreterFor (Scoped param (effect !! ei) !! eo) (effect !! ei : r)
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        Run act ->
          ex . fmap Right <$> liftSem (weave s (inScope . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))
        InScope param main -> do
          exResumable s ex ex' $ raise $ runStop $ withResource param \ resource ->
            interpretResumableH (scopedHandler resource) (raiseUnder (inScope $ wv (wv' (main <$ s') <$ s)))

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedR ::
  ∀ param resource effect eo ei r .
  (∀ x . param -> (resource -> Sem (Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : Stop eo : r) x) ->
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
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : Stop eo : r) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedR_ resource =
  interpretScopedR \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWithH ::
  ∀ extra param resource effect eo ei r r1 extraerr .
  extraerr ~ (extra ++ '[Stop eo]) =>
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  KnownList (extra ++ '[Stop eo]) =>
  (∀ x . param -> (resource -> Sem (extra ++ Stop eo : r) x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (effect !! ei) (Sem r0) (Stop ei : r1) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (Resumable (Weaving (InScope param main) s' wv' ex' _)) s wv ex _ -> do
      exResumable s ex ex' $ runStop $ withResource param \ resource ->
        interpretResumableH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param (effect !! ei) !! eo])
             (singList @(effect !! ei : extraerr))) $ wv (wv' (main <$ s') <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param (effect !! ei) !! eo) (effect !! ei : r1)
    inScope =
      interpretWeaving \ (Weaving (Resumable (Weaving effect s' wv' ex' ins')) s wv ex ins) -> case effect of
        InScope param main ->
          exResumable s ex ex' $
          restack (extendMembershipLeft (singList @(effect !! ei : extraerr))) $
          runStop $
          withResource param \ resource ->
            interpretResumableH (scopedHandler resource) (inScope $ wv (wv' (main <$ s') <$ s))
        Run act ->
          ex . fmap Right <$> liftSem (weave s (inScope . wv) ins (injWeaving (Weaving act s' wv' ex' ins')))

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- continuing the scope execution on resumption.
interpretScopedRWith ::
  ∀ extra param resource effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  KnownList (extra ++ '[Stop eo]) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem (Stop eo : r) x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem (Stop ei : r1) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWith withResource scopedHandler =
  interpretScopedRWithH @extra withResource \ r e -> liftT (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
--   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
--   continuing the scope execution on resumption.
-- - The resource allocator is a plain action.
interpretScopedRWith_ ::
  ∀ extra param effect eo ei r r1 .
  r1 ~ (extra ++ Stop eo : r) =>
  r1 ~ ((extra ++ '[Stop eo]) ++ r) =>
  KnownList (extra ++ '[Stop eo]) =>
  (∀ x . param -> Sem r1 x -> Sem (Stop eo : r) x) ->
  (∀ r0 x . effect (Sem r0) x -> Sem (Stop ei : r1) x) ->
  InterpreterFor (Scoped param (effect !! ei) !! eo) r
interpretScopedRWith_ extra scopedHandler =
  interpretScopedRWith @extra (\ p f -> extra p (f ())) (const scopedHandler)
