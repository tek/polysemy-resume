-- | Description: Polysemy effects for cross-interpreter error tracking.
module Polysemy.Resume (
  -- * Introduction
  -- $intro
  module Polysemy.Resume.Effect.Stop,
  module Polysemy.Resume.Effect.Resumable,

  -- * Resuming a Stopped Computation
  resume,
  (!!),
  interpretResumable,
  interpretResumableH,
  interceptResumable,
  interceptResumableH,
  interceptResumableUsing,
  interceptResumableUsingH,
  resumable,
  raiseResumable,
  resumableIO,
  interpretScopedResumable,
  interpretScopedResumableH,
  interpretScopedResumable_,
  interpretScopedResumableWith,
  interpretScopedResumableWithH,
  interpretScopedResumableWith_,
  interpretResumableScoped,
  interpretResumableScopedH,
  interpretResumableScoped_,
  interpretResumableScopedWith,
  interpretResumableScopedWithH,
  interpretResumableScopedWith_,
  interpretScopedR,
  interpretScopedRH,
  interpretScopedR_,
  interpretScopedRWith,
  interpretScopedRWithH,
  interpretScopedRWith_,

  -- * Partial Handlers
  -- $partial
  resumableOr,

  -- * Various Combinators
  resumeAs,
  (<!),
  (!>),
  resumeWith,
  (!>>),
  resumingWith,
  (<<!),
  resume_,
  resumeHoist,
  resumeHoistAs,
  resuming,
  resumeHoistError,
  resumeHoistErrorAs,
  restop,
  resumeEither,
  resumeMaybe,
  resumeOr,
  resumingOr,
  resumeError,
  resumableError,
  resumableFor,
  runAsResumable,
  catchResumable,
  stopToFailWith,
  stopToFail,
  resumeFailWith,
  resumeFail,
  module Polysemy.Resume.Interpreter.Stop,
) where

import Polysemy.Resume.Effect.Resumable (Resumable, type (!!))
import Polysemy.Resume.Effect.Stop (Stop (..), stop)
import Polysemy.Resume.Interpreter.Resumable (
  catchResumable,
  interceptResumable,
  interceptResumableH,
  interceptResumableUsing,
  interceptResumableUsingH,
  interpretResumable,
  interpretResumableH,
  raiseResumable,
  resumable,
  resumableError,
  resumableFor,
  resumableIO,
  resumableOr,
  runAsResumable,
  )
import Polysemy.Resume.Interpreter.Scoped (
  interpretResumableScoped,
  interpretResumableScopedH,
  interpretResumableScopedWith,
  interpretResumableScopedWithH,
  interpretResumableScopedWith_,
  interpretResumableScoped_,
  interpretScopedR,
  interpretScopedRH,
  interpretScopedRWith,
  interpretScopedRWithH,
  interpretScopedRWith_,
  interpretScopedR_,
  interpretScopedResumable,
  interpretScopedResumableH,
  interpretScopedResumableWith,
  interpretScopedResumableWithH,
  interpretScopedResumableWith_,
  interpretScopedResumable_,
  )
import Polysemy.Resume.Interpreter.Stop (
  mapStop,
  replaceStop,
  runStop,
  showStop,
  stopEither,
  stopEitherAs,
  stopEitherWith,
  stopNote,
  stopOnError,
  stopOnErrorWith,
  stopToError,
  stopToErrorIO,
  stopToErrorWith,
  stopToIOFinal,
  stopTryAny,
  stopTryIO,
  stopTryIOE,
  stopTryIOError,
  )
import Polysemy.Resume.Resume (
  restop,
  resume,
  resumeAs,
  resumeEither,
  resumeError,
  resumeFail,
  resumeFailWith,
  resumeHoist,
  resumeHoistAs,
  resumeHoistError,
  resumeHoistErrorAs,
  resumeMaybe,
  resumeOr,
  resumeWith,
  resume_,
  resuming,
  resumingOr,
  resumingWith,
  stopToFail,
  stopToFailWith,
  (!!),
  (!>),
  (!>>),
  (<!),
  (<<!),
  )

-- $intro
-- This library provides the Polysemy effects 'Resumable' and 'Stop' for the purpose of safely connecting throwing and
-- catching errors across different interpreters.
--
-- Consider the effect:
--
-- @
-- data Stopper :: Effect where
--   StopBang :: Stopper m ()
--   StopBoom :: Stopper m ()
--
-- makeSem ''Stopper
--
-- data Boom =
--   Boom { unBoom :: Text }
--   |
--   Bang { unBang :: Int }
--   deriving stock (Eq, Show)
--
-- interpretStopper ::
--   Member (Error Boom) r =>
--   InterpreterFor Stopper r
-- interpretStopper =
--   interpret \\case
--     StopBang -> throw (Bang 13)
--     StopBoom -> throw (Boom "ouch")
-- @
--
-- If we want to use @Stopper@ in the interpreter of another effect, we have no way of knowing about the errors thrown
-- by its interpreter, even though we can catch @Boom@!
-- This library makes the connection explicit by changing 'Polysemy.Error.Error' to 'Stop' and wrapping @Stopper@ in
-- 'Resumable' when using it in an effect stack:

-- $partial
-- In some cases, the errors thrown by an interpreter contain details about the implementation, which we might want to
-- hide from dependents; or it may throw fatal errors we don't want to handle at all.
-- For this purpose, we can create partial 'Resumable's by transforming errors before handling them:
