module Polysemy.Resume.Resume where

import Polysemy (raiseUnder)
import Polysemy.Error (throw)

import Polysemy.Resume.Data.Resumable (Resumable)
import Polysemy.Resume.Data.Stop (Stop, stop)
import Polysemy.Resume.Resumable (runAsResumable)
import Polysemy.Resume.Stop (runStop)

-- |Execute the action of a regular effect @eff@ so that any error of type @err@ that maybe be thrown by the (unknown)
-- interpreter used for @eff@ will be caught here and handled by the @handler@ argument.
-- This is similar to 'Polysemy.Error.catch' with the additional guarantee that the error will have to be explicitly
-- matched, therefore preventing accidental failure to handle an error and bubbling it up to @main@.
-- It also imposes a membership of @Resumable err eff@ on the program, requiring the interpreter for @eff@ to be adapted
-- with 'Polysemy.Resume.Resumable.resumable'.
--
-- @
-- data Resumer :: Effect where
--   MainProgram :: Resumer m Int
--
-- makeSem ''Resumer
--
-- interpretResumer ::
--   Member (Resumable Boom Stopper) r =>
--   InterpreterFor Resumer r
-- interpretResumer =
--   interpret \\ MainProgram ->
--     resume (192 \<$ stopBang) \\ _ ->
--       pure 237
-- @
resume ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  Sem (eff : r) a ->
  (err -> Sem r a) ->
  Sem r a
resume sem handler =
  either handler pure =<< runStop (runAsResumable (raiseUnder sem))
{-# INLINE resume #-}

-- |Flipped variant of 'resume'.
resuming ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  (err -> Sem r a) ->
  Sem (eff : r) a ->
  Sem r a
resuming =
  flip resume
{-# INLINE resuming #-}

-- |Variant of 'resume' that unconditionally recovers with a constant value.
resumeAs ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  a ->
  Sem (eff : r) a ->
  Sem r a
resumeAs a =
  resuming \ _ -> pure a
{-# INLINE resumeAs #-}

-- |Variant of 'resume' that propagates the error to another 'Stop' effect after applying a function.
resumeHoist ::
  ∀ err err' eff r a .
  Members [Resumable err eff, Stop err'] r =>
  (err -> err') ->
  Sem (eff : r) a ->
  Sem r a
resumeHoist f =
  resuming (stop . f)
{-# INLINE resumeHoist #-}

-- |Variant of 'resumeHoist' that uses a constant value.
resumeHoistAs ::
  ∀ err err' eff r a .
  Members [Resumable err eff, Stop err'] r =>
  err' ->
  Sem (eff : r) a ->
  Sem r a
resumeHoistAs err =
  resumeHoist (const err)
{-# INLINE resumeHoistAs #-}

-- |Variant of 'resumeHoist' that uses the unchanged error.
restop ::
  ∀ err eff r a .
  Members [Resumable err eff, Stop err] r =>
  Sem (eff : r) a ->
  Sem r a
restop =
  resumeHoist id
{-# INLINE restop #-}

-- |Variant of 'resume' that propagates the error to an 'Error' effect after applying a function.
resumeHoistError ::
  ∀ err err' eff r a .
  Members [Resumable err eff, Error err'] r =>
  (err -> err') ->
  Sem (eff : r) a ->
  Sem r a
resumeHoistError f =
  resuming (throw . f)
{-# INLINE resumeHoistError #-}

-- |Variant of 'resumeHoistError' that uses the unchanged error.
resumeHoistErrorAs ::
  ∀ err err' eff r a .
  Members [Resumable err eff, Error err'] r =>
  err' ->
  Sem (eff : r) a ->
  Sem r a
resumeHoistErrorAs err =
  resumeHoistError (const err)
{-# INLINE resumeHoistErrorAs #-}

-- |Variant of 'resumeHoistError' that uses the unchanged error.
resumeError ::
  ∀ err eff r a .
  Members [Resumable err eff, Error err] r =>
  Sem (eff : r) a ->
  Sem r a
resumeError =
  resumeHoistError id
{-# INLINE resumeError #-}
