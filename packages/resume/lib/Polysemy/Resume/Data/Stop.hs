{-# options_haddock prune #-}

-- |Description: Internal
module Polysemy.Resume.Data.Stop where

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
  -- |Abort a computation with an error value.
  Stop :: e -> Stop e m a

makeSem ''Stop
