{-# options_haddock prune #-}

-- |Description: The 'Stop' effect.
module Polysemy.Resume.Effect.RunStop where

import Polysemy.Resume.Effect.Stop (Stop)

newtype RunStop m a =
  RunStop { unRunStop :: Scoped1 Stop (Const ()) Either m a }
