module Polysemy.Resume.Data.Resumable where

import Polysemy.Internal.Union (Weaving)

-- |Effect that wraps another effect @eff@, marking it as throwing errors of type @err@ using
-- 'Polysemy.Resume.Data.Stop.Stop'.
data Resumable err eff m a where
  Resumable ::
    ∀ err eff r a .
    Weaving eff (Sem r) a ->
    Resumable err eff (Sem r) (Either err a)
