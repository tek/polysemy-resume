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
  EffHandlerH effect (extra ++ Opaque q1 : Stop err : Opaque q2 : r)

type SRHandler effect resource err extra r =
  ∀ q1 q2 z x . resource -> effect z x -> Sem (extra ++ Opaque q1 : Stop err : Opaque q2 : r) x

runScopedResumable ::
  ∀ effect param err r .
  Member RunStop r =>
  (∀ q1 q2 x . param -> Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x -> Sem (Opaque q1 : Stop err : Opaque q2 : r) x) ->
  InterpreterFor (Scoped effect param !! err) r
runScopedResumable scopedInterpreter =
  runResumable \ sem -> runScoped scopedInterpreter sem

runScopedResumable_ ::
  ∀ effect param err r .
  Member RunStop r =>
  (∀ q1 q2 x . param -> Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x -> Sem (Opaque q1 : Stop err : Opaque q2 : r) x) ->
  InterpreterFor (Scoped effect param !! err) r
runScopedResumable_ scopedInterpreter =
  runResumable \ sem -> runScoped scopedInterpreter sem

-- |Combined higher-order interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumableH ::
  ∀ effect param resource err r .
  Member RunStop r =>
  SRAlloc param resource err '[] r ->
  (∀ q1 q2 . resource -> EffHandlerH effect (Opaque q1 : Stop err : Opaque q2 : r)) ->
  InterpreterFor (Scoped effect param !! err) r
interpretScopedResumableH withResource scopedHandler =
  runScopedResumable \ param sem ->
    withResource param \ r ->
      interpretH (scopedHandler r) sem

-- |Combined interpreter for 'Scoped' and 'Resumable'.
-- This allows 'Stop' to be sent from within the resource allocator so that the consumer receives it, terminating the
-- entire scope.
interpretScopedResumable ::
  ∀ effect param resource err r .
  Member RunStop r =>
  SRAlloc param resource err '[] r ->
  (∀ z x . resource -> effect z x -> Sem (Stop err : r) x) ->
  InterpreterFor (Scoped effect param !! err) r
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
  InterpreterFor (Scoped effect param !! err) r
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
  InterpreterFor (Scoped effect param !! err) r
interpretScopedResumableWithH withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x) ->
    withResource param \ resource ->
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
  InterpreterFor (Scoped effect param !! err) r
interpretScopedResumableWith withResource scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x) ->
    withResource param \ resource ->
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
  InterpreterFor (Scoped effect param !! err) r
interpretScopedResumableWith_ extra scopedHandler =
  runScopedResumable \ param (sem :: Sem (effect : Opaque q1 : Stop err : Opaque q2 : r) x) ->
    extra @q1 @q2 param $
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
  (∀ q1 q2 . resource -> EffHandlerH effect (Stop err : Opaque q1 : Opaque q2 : r)) ->
  InterpreterFor (Scoped (effect !! err) param) r
interpretResumableScopedH withResource scopedHandler =
  runScoped \ param sem ->
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
  InterpreterFor (Scoped (effect !! err) param) r
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
  InterpreterFor (Scoped (effect !! err) param) r
interpretResumableScoped_ resource =
  interpretResumableScoped \ p use -> use =<< raise (resource p)
