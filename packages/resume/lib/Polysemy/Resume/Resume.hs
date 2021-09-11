module Polysemy.Resume.Resume where

import Polysemy (raiseUnder, raiseUnder2)
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
  either handler pure =<< runStop (runAsResumable @err (raiseUnder sem))
{-# inline resume #-}

-- |Operator version of 'resume'.
(!!) ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  Sem (eff : r) a ->
  (err -> Sem r a) ->
  Sem r a
(!!) =
  resume
{-# inline (!!) #-}

-- Reinterpreting version of 'resume'.
resumeRe ::
  ∀ err eff r a .
  Sem (eff : r) a ->
  (err -> Sem (Resumable err eff : r) a) ->
  Sem (Resumable err eff : r) a
resumeRe sem handler =
  either handler pure =<< runStop (runAsResumable @err (raiseUnder2 sem))
{-# inline resumeRe #-}

-- |Flipped variant of 'resume'.
resuming ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  (err -> Sem r a) ->
  Sem (eff : r) a ->
  Sem r a
resuming =
  flip resume
{-# inline resuming #-}

-- |Flipped variant of 'resumeRe'.
resumingRe ::
  ∀ err eff r a .
  (err -> Sem (Resumable err eff : r) a) ->
  Sem (eff : r) a ->
  Sem (Resumable err eff : r) a
resumingRe =
  flip resumeRe
{-# inline resumingRe #-}

-- |Variant of 'resume' that unconditionally recovers with a constant value.
resumeAs ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  a ->
  Sem (eff : r) a ->
  Sem r a
resumeAs a =
  resuming @err \ _ -> pure a
{-# inline resumeAs #-}

-- |Operator version of 'resumeAs'.
(<!) ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  a ->
  Sem (eff : r) a ->
  Sem r a
(<!) =
  resumeAs @err

-- |Convenience specialization of 'resume' that silently discards errors for void programs.
resume_ ::
  ∀ err eff r .
  Member (Resumable err eff) r =>
  Sem (eff : r) () ->
  Sem r ()
resume_ =
  resumeAs @err ()

-- |Variant of 'resume' that propagates the error to another 'Stop' effect after applying a function.
resumeHoist ::
  ∀ err eff err' r a .
  Members [Resumable err eff, Stop err'] r =>
  (err -> err') ->
  Sem (eff : r) a ->
  Sem r a
resumeHoist f =
  resuming (stop . f)
{-# inline resumeHoist #-}

-- |Variant of 'resumeHoist' that uses a constant value.
resumeHoistAs ::
  ∀ err eff err' r .
  Members [Resumable err eff, Stop err'] r =>
  err' ->
  InterpreterFor eff r
resumeHoistAs err =
  resumeHoist @err (const err)
{-# inline resumeHoistAs #-}

-- |Variant of 'resumeHoist' that uses the unchanged error.
restop ::
  ∀ err eff r .
  Members [Resumable err eff, Stop err] r =>
  InterpreterFor eff r
restop =
  resumeHoist @err id
{-# inline restop #-}

-- |Variant of 'resume' that immediately produces an 'Either'.
resumeEither ::
  ∀ err eff r a .
  Member (Resumable err eff) r =>
  Sem (eff : r) a ->
  Sem r (Either err a)
resumeEither ma =
  resuming (pure . Left) (Right <$> ma)

-- |Variant of 'resume' that takes a branch for error and success.
-- This allows the success branch to contain other resumptions.
resumeOr ::
  ∀ err eff r a b .
  Member (Resumable err eff) r =>
  Sem (eff : r) a ->
  (a -> Sem r b) ->
  (err -> Sem r b) ->
  Sem r b
resumeOr ma fb err =
  resumeEither ma >>= \case
    Right a -> fb a
    Left e -> err e

-- |Variant of 'resuming' that takes a branch for error and success.
-- This allows the success branch to contain other resumptions.
resumingOr ::
  ∀ err eff r a b .
  Member (Resumable err eff) r =>
  (err -> Sem r b) ->
  Sem (eff : r) a ->
  (a -> Sem r b) ->
  Sem r b
resumingOr err ma fb =
  resumeOr ma fb err

-- |Variant of 'resume' that propagates the error to an 'Error' effect after applying a function.
resumeHoistError ::
  ∀ err eff err' r a .
  Members [Resumable err eff, Error err'] r =>
  (err -> err') ->
  Sem (eff : r) a ->
  Sem r a
resumeHoistError f =
  resuming (throw . f)
{-# inline resumeHoistError #-}

-- |Variant of 'resumeHoistError' that uses the unchanged error.
resumeHoistErrorAs ::
  ∀ err eff err' r a .
  Members [Resumable err eff, Error err'] r =>
  err' ->
  Sem (eff : r) a ->
  Sem r a
resumeHoistErrorAs err =
  resumeHoistError @err (const err)
{-# inline resumeHoistErrorAs #-}

-- |Variant of 'resumeHoistError' that uses the unchanged error.
resumeError ::
  ∀ err eff r a .
  Members [Resumable err eff, Error err] r =>
  Sem (eff : r) a ->
  Sem r a
resumeError =
  resumeHoistError @err id
{-# inline resumeError #-}
