module Polysemy.Resume.Data.Stop where

import Polysemy (makeSem)

-- |An effect similar to 'Polysemy.Error.Error' without the ability to be caught.
-- Used to signal that an error is supposed to be expected by dependent programs.
--
-- @
-- interpretStopper ::
--   Member (Stop Boom) r =>
--   InterpreterFor Stopper r
-- interpretStopper =
--   interpret \\case
--     StopBang -> stop (Bang 13)
--     StopBoom -> stop (Boom "ouch")
-- @
data Stop e :: Effect where
  Stop :: e -> Stop e m a

makeSem ''Stop
