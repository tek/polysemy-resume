-- |Description: The 'Resumable' effect.
module Polysemy.Resume.Effect.Resumable where

-- |Effect that wraps another effect @eff@, marking it as throwing errors of type @err@ using
-- 'Polysemy.Resume.Effect.Stop.Stop'.
type Resumable :: Type -> Effect -> Effect
newtype Resumable err eff m a =
  Resumable { unResumable :: Scoped eff () (Either err) m a }

-- |Infix alias for 'Resumable'.
--
-- @
-- Member (Stopper !! Boom) r =>
-- @
type (!!) :: Effect -> Type -> Effect
type eff !! err =
  Resumable err eff
