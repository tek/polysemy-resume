module Polysemy.Resume.Data.Resumable where

-- |Effect that wraps another effect @eff@, marking it as throwing errors of type @err@ using
-- 'Polysemy.Resume.Data.Stop.Stop'.
data Resumable err eff :: Effect where
  Resumable :: ∀ err eff m a . eff m a -> Resumable err eff m (Either err a)

-- |Infix alias for 'Resumable'.
--
-- @
-- Member (Stopper !! Boom) r =>
-- @
type eff !! err =
  Resumable err eff
