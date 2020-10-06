module Polysemy.Resume.ResumeTest where

import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

import Polysemy.Resume (Resumable, Stop, errorStop, resumable, resumableFor, resume, runStop, stop)

data Stopper :: Effect where
  StopBang :: Stopper m ()
  StopBoom :: Stopper m ()

makeSem ''Stopper

data Resumer :: Effect where
  DoB :: Resumer m Int

makeSem ''Resumer

data Boom =
  Boom { unBoom :: Text }
  |
  Bang { unBang :: Int }
  deriving (Eq, Show, Generic)

newtype Blip =
  Blip { unBlip :: Int }
  deriving (Eq, Show, Generic)

bangOnly :: Boom -> Maybe Blip
bangOnly = \case
  Bang n -> Just (Blip n)
  Boom _ -> Nothing

interpretAError ::
  Member (Error Boom) r =>
  InterpreterFor Stopper r
interpretAError =
  interpret \case
    StopBang -> throw (Bang (-413))
    StopBoom -> throw (Boom "ouch")

interpretAResumable ::
  InterpreterFor (Resumable Boom Stopper) r
interpretAResumable =
  resumable (errorStop . interpretAError . raiseUnder)

interpretAStop ::
  Member (Stop Boom) r =>
  InterpreterFor Stopper r
interpretAStop =
  interpret \case
    StopBang -> stop (Bang 13)
    StopBoom -> stop (Boom "ouch")

interpretAResumableBlip ::
  Member (Stop Boom) r =>
  InterpreterFor (Resumable Blip Stopper) r
interpretAResumableBlip =
  resumableFor bangOnly (interpretAStop)

interpretBPartialUnhandled ::
  Member (Resumable Blip Stopper) r =>
  InterpreterFor Resumer r
interpretBPartialUnhandled =
  interpret \ DoB ->
    resume (192 <$ stopBoom) \ (Blip num) ->
      pure (num * 3)

interpretBPartial ::
  Member (Resumable Blip Stopper) r =>
  InterpreterFor Resumer r
interpretBPartial =
  interpret \ DoB ->
    resume (192 <$ stopBang) \ (Blip num) ->
      pure (num * 3)

interpretB ::
  Member (Resumable Boom Stopper) r =>
  InterpreterFor Resumer r
interpretB =
  interpret \ DoB ->
    resume (192 <$ stopBang) \ _ ->
      pure 237

test_resume :: UnitTest
test_resume =
  runTestAuto do
    assertRight 39 =<< runStop (interpretAResumableBlip (interpretBPartial doB))
    (Left (Boom "ouch") ===) =<< runStop (interpretAResumableBlip (interpretBPartialUnhandled doB))
    (237 ===) =<< interpretAResumable (interpretB doB)
