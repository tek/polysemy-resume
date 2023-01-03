{-# options_haddock prune #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- |Description: Interpreters for 'Scoped' in combination with 'Resumable'
--
-- @since 0.6.0.0
module Polysemy.Resume.Interpreter.Scoped where

import GHC.Err (errorWithoutStackTrace)
import Polysemy.HigherOrder (exposeH, propagate)
import Polysemy.Internal (mapMembership)
import Polysemy.Internal.HigherOrder (controlWithProcessorH)
import Polysemy.Internal.Scoped (OuterRun (OuterRun))
import Polysemy.Internal.Sing (KnownList (singList))
import Polysemy.Internal.Union (injectMembership)
import Polysemy.Internal.WeaveClass (StT)
import Polysemy.Membership (ElemOf (Here, There), subsumeUsing)
import Polysemy.Opaque (Opaque (Opaque))

import Polysemy.Resume.Effect.Resumable (Resumable (Resumable), type (!!))
import Polysemy.Resume.Effect.RunStop (RunStop)
import Polysemy.Resume.Effect.Stop (Stop)
import Polysemy.Resume.Interpreter.Resumable (interpretResumableH, runResumable, runResumableWith)
import Polysemy.Resume.Interpreter.Stop (runStop)

type SRAlloc param resource err extra r =
  ∀ q1 q2 x .
  param ->
  (resource -> Sem (extra ++ Opaque q1 : Stop err : Opaque q2 : r) x) ->
  Sem (Opaque q1 : Stop err : Opaque q2 : r) x

type SRHandlerH effect resource err extra r =
  ∀ q1 q2 .
  resource ->
  EffHandlerH effect (extra ++ Opaque q1 : Stop err : Opaque q2 : r) (extra ++ Opaque q1 : Stop err : Opaque q2 : r)

type SRHandler effect resource err extra r =
  ∀ q1 q2 z x . resource -> effect z x -> Sem (extra ++ Opaque q1 : Stop err : Opaque q2 : r) x

runScopedResumable ::
  ∀ effect param modifier err r .
  Member RunStop r =>
  (∀ q1 q2 x . param -> Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x -> Sem (Opaque q1 : Stop err : Opaque q2 : r) (modifier x)) ->
  InterpreterFor (Scoped effect param modifier !! err) r
runScopedResumable scopedInterpreter =
  runResumable \ sem -> runScoped scopedInterpreter sem

runScopedResumable_ ::
  ∀ effect param err r .
  Member RunStop r =>
  (∀ q1 q2 x . param -> Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x -> Sem (Opaque q1 : Stop err : Opaque q2 : r) x) ->
  InterpreterFor (Scoped_ effect param !! err) r
runScopedResumable_ scopedInterpreter =
  runResumable \ sem -> runScoped_ scopedInterpreter sem

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ effect param resource err r .
  Member RunStop r =>
  SRAlloc param resource err '[] r ->
  (∀ q1 q2 . resource -> EffHandlerH effect (Opaque q1 : Stop err : Opaque q2 : r) (Opaque q1 : Stop err : Opaque q2 : r)) ->
  InterpreterFor (Scoped_ effect param !! err) r
interpretScopedResumableH withResource scopedHandler =
  runScopedResumable \ param sem ->
    Identity <$> withResource param \ r ->
      interpretH (scopedHandler r) sem

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ effect param resource err r .
  Member RunStop r =>
  SRAlloc param resource err '[] r ->
  (∀ z x . resource -> effect z x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped_ effect param !! err) r
interpretScopedResumable withResource scopedHandler =
  interpretScopedResumableH withResource \ r e -> subsume_ (scopedHandler r e)

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, the resource allocator is a plain action.
interpretScopedResumable_ ::
  ∀ effect param resource err r .
  Member RunStop r =>
  (∀ q1 q2 . param -> Sem (Opaque q1 : Stop err : Opaque q2 : r) resource) ->
  (∀ z x . resource -> effect z x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped_ effect param !! err) r
interpretScopedResumable_ resource =
  interpretScopedResumable \ p use -> use =<< resource p

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWithH ::
  ∀ extra effect param resource err r .
  KnownList extra =>
  Member RunStop r =>
  SRAlloc param resource err extra r ->
  SRHandlerH effect resource err extra r ->
  InterpreterFor (Scoped_ effect param !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x) ->
    Identity <$> withResource param \ resource ->
      interpretH (scopedHandler @q1 @q2 resource) $
      mapMembership (injectMembership (singList @'[effect]) (singList @extra)) $
      sem

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableWith ::
  ∀ extra param resource effect err r .
  KnownList extra =>
  Member RunStop r =>
  SRAlloc param resource err extra r ->
  SRHandler effect resource err extra r ->
  InterpreterFor (Scoped_ effect param !! err) r
interpretScopedResumableWith withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x) ->
    Identity <$> withResource param \ resource ->
      sem
      & mapMembership (injectMembership (singList @'[effect]) (singList @extra))
      & interpretH \ e -> raise (scopedHandler @q1 @q2 resource e)

-- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- interpreted by the resource allocator.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
-- In this variant, no resource is used and the allocator is a plain interpreter.
interpretScopedResumableWith_ ::
  ∀ extra param effect err r .
  KnownList extra =>
  Member RunStop r =>
  (∀ q1 q2 x . param -> Sem (extra ++ Opaque q1 : Stop err : Opaque q2 : r) x -> Sem (Opaque q1 : Stop err : Opaque q2 : r) x) ->
  (∀ q1 q2 z x . effect z x -> Sem (extra ++ Opaque q1 : Stop err : Opaque q2 : r) x) ->
  InterpreterFor (Scoped_ effect param !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x) ->
    fmap Identity $ extra @q1 @q2 param $
      sem
      & mapMembership (injectMembership (singList @'[effect]) (singList @extra))
      & interpretH \ e -> raise (scopedHandler @q1 @q2 e)

-- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScopedH ::
  ∀ param resource effect err r .
  Member RunStop r =>
  (∀ q x . param -> (resource -> Sem (Opaque q : r) x) -> Sem (Opaque q : r) x) ->
  (∀ q1 q2 . resource -> EffHandlerH effect (Opaque q1 : Opaque q2 : r) (Stop err : Opaque q1 : Opaque q2 : r)) ->
  InterpreterFor (Scoped_ (effect !! err) param) r
interpretResumableScopedH withResource scopedHandler =
  runScoped_ \ param sem ->
    withResource param \ r ->
      interpretResumableH (scopedHandler r) sem

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- scope.
interpretResumableScoped ::
  ∀ param resource effect err r .
  Member RunStop r =>
  (∀ q x . param -> (resource -> Sem (Opaque q : r) x) -> Sem (Opaque q : r) x) ->
  (∀ q z x . resource -> effect z x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped_ (effect !! err) param) r
interpretResumableScoped withResource scopedHandler =
  interpretResumableScopedH withResource \ r e -> raise (raiseUnder2 (scopedHandler r e))

-- |Combined interpreter for 'Resumable' and 'Scoped'.
-- In this variant:
-- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- - The resource allocator is a plain action.
interpretResumableScoped_ ::
  ∀ param resource effect err r .
  Member RunStop r =>
  (param -> Sem r resource) ->
  (∀ q z x . resource -> effect z x -> Sem (Stop err : Opaque q : r) x) ->
  InterpreterFor (Scoped_ (effect !! err) param) r
interpretResumableScoped_ resource =
  interpretResumableScoped \ p use -> use =<< raise (resource p)

runResumableScoped ::
  (∀ q x . param -> Sem (effect !! err : Opaque q : r) x -> Sem (Opaque q : r) x) ->
  InterpreterFor (Scoped_ (effect !! err) param) r
runResumableScoped wr =
  runScoped_ wr

-- -- |Combined higher-order interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects
-- -- that are interpreted by the resource allocator.
-- -- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- -- scope.
-- interpretResumableScopedWithH ::
--   ∀ extra param resource effect err r .
--   KnownList extra =>
--   Member RunStop r =>
--   Member RunStop (extra ++ r) =>
--   (∀ q x . param -> (resource -> Sem (extra ++ Opaque q : r) x) -> Sem (Opaque q : r) x) ->
--   -- (∀ q t z x . Traversable t => resource -> effect z x -> Sem (HigherOrder z t (Resumable err effect) (extra ++ Opaque q : r) (extra ++ Opaque q : r) : Stop err : extra ++ Opaque q : r) x) ->
--   (∀ q1 q2 t z x . Traversable t => resource -> effect z x -> Sem (HigherOrder z t effect (Opaque q1 : extra ++ Opaque q2 : r) (Stop err : Opaque q1 : extra ++ Opaque q2 : r) : Stop err : extra ++ Opaque q2 : r) x) ->
--   InterpreterFor (Scoped_ (effect !! err) param) r
-- interpretResumableScopedWithH withResource scopedHandler =
--   runScoped \ param (sem :: Sem (effect !! err : Opaque q1 : r) x) ->
--     Identity <$> withResource param \ resource ->
--       sem
--         & mapMembership (injectMembership (singList @'[effect !! err]) (singList @extra))
--         & interpretResumableH (scopedHandler resource)
--   -- runScoped \ param (sem :: Sem (effect !! err : Opaque q : r) x) ->
--   --   withResource param \ resource ->
--   --     sem
--   --       & mapMembership (injectMembership (singList @'[effect !! err]) (singList @extra))
--   --       & interpretResumableH (scopedHandler @q resource)

-- -- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- -- interpreted by the resource allocator.
-- -- In this variant, only the handler may send 'Stop', but this allows resumption to happen on each action inside of the
-- -- scope.
-- interpretResumableScopedWith ::
--   ∀ extra param resource effect err r .
--   KnownList extra =>
--   (∀ q x . param -> (resource -> Sem (extra ++ Opaque q : r) x) -> Sem (Opaque q : r) x) ->
--   (∀ z x . resource -> effect z x -> Sem (Stop err : (extra ++ r)) x) ->
--   InterpreterFor (Scoped param (effect !! err)) r
-- interpretResumableScopedWith withResource scopedHandler =
--   runScopedNew \ param (sem :: Sem (effect !! err : Opaque q : r) x) ->
--     withResource param \resource ->
--       sem
--         & mapMembership (injectMembership (singList @'[effect !! err]) (singList @extra))
--         & interpretResumableH \ e ->
--           raise $
--           mapMembership (injectMembership @r (singList @(Stop err : extra)) (singList @'[Opaque q])) (scopedHandler resource e)

-- -- |Combined interpreter for 'Resumable' and 'Scoped' that allows the handler to use additional effects that are
-- -- interpreted by the resource allocator.
-- -- In this variant:
-- -- - Only the handler may send 'Stop', but this allows resumption to happen on each action inside of the scope.
-- -- - No resource is used and the allocator is a plain interpreter.
-- interpretResumableScopedWith_ ::
--   ∀ extra param effect err r .
--   KnownList extra =>
--   (∀ q x . param -> Sem (extra ++ Opaque q : r) x -> Sem (Opaque q : r) x) ->
--   (∀ z x . effect z x -> Sem (Stop err : (extra ++ r)) x) ->
--   InterpreterFor (Scoped param (effect !! err)) r
-- interpretResumableScopedWith_ extra scopedHandler =
--   interpretResumableScopedWith @extra @param @() @effect @err @r (\ p f -> extra p (f ())) (const scopedHandler)

-- -- |Combined higher-order interpreter for 'Resumable' and 'Scoped'.
-- -- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- -- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- -- continuing the scope execution on resumption.
-- interpretScopedRH ::
--   ∀ param resource effect eo ei r .
--   (∀ q x . param -> (resource -> Sem (Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
--   (∀ q t z x . Traversable t => resource -> effect z x -> Sem (HigherOrder z t (effect !! ei) (Stop eo : Opaque q : r) (Stop eo : Opaque q : r) : Stop ei : Stop eo : Opaque q : r) x) ->
--   InterpreterFor (Scoped param (effect !! ei) !! eo) r
-- interpretScopedRH withResource scopedHandler =
--   runScopedResumable \ param sem ->
--     withResource param \ r ->
--       interpretResumableH (scopedHandler r) sem

-- -- |Combined interpreter for 'Scoped' and 'Resumable'.
-- -- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- -- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- -- continuing the scope execution on resumption.
-- interpretScopedR ::
--   ∀ param resource effect eo ei r .
--   (∀ q x . param -> (resource -> Sem (Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
--   (∀ q z x . resource -> effect z x -> Sem (Stop ei : Stop eo : Opaque q : r) x) ->
--   InterpreterFor (Scoped param (effect !! ei) !! eo) r
-- interpretScopedR withResource scopedHandler =
--   interpretScopedRH withResource \ r e -> raise (scopedHandler r e)

-- -- |Combined interpreter for 'Scoped' and 'Resumable'.
-- -- In this variant:
-- -- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- --   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- --   continuing the scope execution on resumption.
-- -- - The resource allocator is a plain action.
-- interpretScopedR_ ::
--   ∀ param resource effect eo ei r .
--   (param -> Sem (Stop eo : r) resource) ->
--   (∀ q z x . resource -> effect z x -> Sem (Stop ei : Stop eo : Opaque q : r) x) ->
--   InterpreterFor (Scoped param (effect !! ei) !! eo) r
-- interpretScopedR_ resource =
--   interpretScopedR \ p use -> use =<< raiseUnder (resource p)

-- -- |Combined higher-order interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects
-- -- that are interpreted by the resource allocator.
-- -- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- -- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- -- continuing the scope execution on resumption.
-- interpretScopedRWithH ::
--   ∀ extra param resource effect eo ei r .
--   KnownList extra =>
--   (∀ q x . param -> (resource -> Sem (extra ++ Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
--   (∀ q t z x . Traversable t => resource -> effect z x -> Sem (HigherOrder z t (effect !! ei) (extra ++ Stop eo : Opaque q : r) (extra ++ Stop eo : Opaque q : r) : Stop ei : extra ++ Stop eo : Opaque q : r) x) ->
--   -- (∀ q z x . resource -> EffHandlerH (effect !! ei) (Stop ei : (extra ++ Stop eo : Opaque q : r)) (Stop ei : (extra ++ Stop eo : Opaque q : r))) ->
--   InterpreterFor (Scoped param (effect !! ei) !! eo) r
-- interpretScopedRWithH withResource scopedHandler =
--   runScopedResumable \ param (sem :: Sem (effect !! ei : Stop eo : Opaque q : r) x) ->
--     withResource param \ resource ->
--       sem
--         & mapMembership (injectMembership (singList @'[effect !! ei]) (singList @extra))
--         & interpretResumableH (scopedHandler @q resource)

-- -- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- -- interpreted by the resource allocator.
-- -- In this variant, both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- -- resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- -- continuing the scope execution on resumption.
-- interpretScopedRWith ::
--   ∀ extra param resource effect eo ei r .
--   KnownList extra =>
--   (∀ q x . param -> (resource -> Sem (extra ++ Stop eo : Opaque q : r) x) -> Sem (Stop eo : Opaque q : r) x) ->
--   (∀ q z x . resource -> effect z x -> Sem (Stop ei : (extra ++ Stop eo : Opaque q : r)) x) ->
--   InterpreterFor (Scoped param (effect !! ei) !! eo) r
-- interpretScopedRWith withResource scopedHandler =
--   runScopedResumable \ param (sem :: Sem (effect !! ei : Stop eo : Opaque q : r) x) ->
--     withResource param \ resource ->
--       sem
--       & mapMembership (injectMembership (singList @'[effect !! ei]) (singList @extra))
--       & interpretResumableH \ e ->
--         raise (scopedHandler @q resource e)

-- -- |Combined interpreter for 'Scoped' and 'Resumable' that allows the handler to use additional effects that are
-- -- interpreted by the resource allocator.
-- -- - Both the handler and the scope may send different errors via 'Stop', encoding the concept that the
-- --   resource allocation may fail to prevent the scope from being executed, and each individual scoped action may fail,
-- --   continuing the scope execution on resumption.
-- -- - The resource allocator is a plain action.
-- interpretScopedRWith_ ::
--   ∀ extra param effect eo ei r .
--   KnownList extra =>
--   (∀ q x . param -> Sem (extra ++ Stop eo : Opaque q : r) x -> Sem (Stop eo : Opaque q : r) x) ->
--   (∀ q z x . effect z x -> Sem (Stop ei : (extra ++ Stop eo : Opaque q : r)) x) ->
--   InterpreterFor (Scoped param (effect !! ei) !! eo) r
-- interpretScopedRWith_ extra scopedHandler =
--   runScopedResumable \ param (sem :: Sem (effect !! ei : Stop eo : Opaque q : r) x) ->
--     extra param do
--       sem
--         & mapMembership (injectMembership (singList @'[effect !! ei]) (singList @extra))
--         & interpretResumableH \ e ->
--           raise (scopedHandler @q e)
