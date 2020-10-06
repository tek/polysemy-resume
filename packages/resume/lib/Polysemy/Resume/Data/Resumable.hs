module Polysemy.Resume.Data.Resumable where

import Polysemy.Internal.Union (Weaving)

data Resumable err eff m a where
  Resumable ::
    ∀ err eff r a .
    Weaving eff (Sem r) a ->
    Resumable err eff (Sem r) (Either err a)
