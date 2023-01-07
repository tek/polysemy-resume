{-# options_haddock prune #-}

-- |Description: The 'Stop' effect.
module Polysemy.Resume.Effect.RunStop where

import Polysemy.Meta (Meta, MetaEffect, (:%))

import Polysemy.Resume.Effect.Stop (Stop)

data RunStopMeta :: MetaEffect where
  RunStopMeta :: z a -> RunStopMeta '[z :% Stop e] m (Either e a)

newtype RunStop m a =
  RunStop { unRunStop :: Meta RunStopMeta m a }
