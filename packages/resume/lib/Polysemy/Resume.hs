module Polysemy.Resume (
  -- * Introduction
  -- $intro
  module Polysemy.Resume.Data.Stop,
  module Polysemy.Resume.Data.Resumable,
  -- * Resuming a Stopped Computation
  resume,
  (!!),
  -- interpretResumable,
  interpretResumableH,
  resumable,
  -- raiseResumable,
  -- resumableIO,
  -- * Partial Handlers
  -- $partial
  -- resumableOr,
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
  resumeOr,
  resumingOr,
  resumeError,
  -- resumableError,
  resumableFor,
  runAsResumable,
  -- catchResumable,
  module Polysemy.Resume.Stop,
) where

import Polysemy.Resume.Data.Resumable (Resumable, type (!!))
import Polysemy.Resume.Data.Stop (Stop (..), stop)
import Polysemy.Resume.Resumable (
  -- catchResumable,
  -- interpretResumable,
  -- raiseResumable,
  resumable,
  -- resumableError,
  resumableFor,
  -- resumableIO,
  -- resumableOr,
  interpretResumableH,
  runAsResumable,
  )
import Polysemy.Resume.Resume (
  restop,
  resume,
  resumeAs,
  resumeEither,
  resumeError,
  resumeHoist,
  resumeHoistAs,
  resumeHoistError,
  resumeHoistErrorAs,
  resumeOr,
  resumeWith,
  resume_,
  resuming,
  resumingOr,
  resumingWith,
  (!!),
  (!>),
  (!>>),
  (<!),
  (<<!),
  )
import Polysemy.Resume.Stop (
  mapStop,
  runStop,
  showStop,
  stopEither,
  stopEitherWith,
  stopNote,
  stopOnError,
  stopOnErrorWith,
  stopToError,
  stopToIOFinal,
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
--   deriving (Eq, Show)
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
-- This library makes the connection explicit by changing 'Polysemy.Error.Error' to 'Stop' and wrapping 'Stopper' in
-- 'Resumable' when using it in an effect stack:

-- $partial
-- In some cases, the errors thrown by an interpreter contain details about the implementation, which we might want to
-- hide from dependents; or it may throw fatal errors we don't want to handle at all.
-- For this purpose, we can create partial 'Resumable's by transforming errors before handling them:
