module Polysemy.Resume.ExampleTest where

import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

import Polysemy.Resume (Resumable, Stop, resumable, resumableFor, resume, runStop, stop)

data Stopper :: Effect where
  StopBang :: Stopper m ()
  StopBoom :: Stopper m ()

makeSem ''Stopper

data Resumer :: Effect where
  MainProgram :: Resumer m Int

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

interpretStopper ::
  Member (Stop Boom) r =>
  InterpreterFor Stopper r
interpretStopper =
  interpret \case
    StopBang -> stop (Bang 13)
    StopBoom -> stop (Boom "ouch")

interpretResumerPartialUnhandled ::
  Member (Resumable Blip Stopper) r =>
  InterpreterFor Resumer r
interpretResumerPartialUnhandled =
  interpret \ MainProgram ->
    resume (192 <$ stopBoom) \ (Blip num) ->
      pure (num * 3)

interpretResumerPartial ::
  Member (Resumable Blip Stopper) r =>
  InterpreterFor Resumer r
interpretResumerPartial =
  interpret \ MainProgram ->
    resume (192 <$ stopBang) \ (Blip num) ->
      pure (num * 3)

interpretResumer ::
  Member (Resumable Boom Stopper) r =>
  InterpreterFor Resumer r
interpretResumer =
  interpret \ MainProgram ->
    resume (192 <$ stopBang) \ _ ->
      pure 237

test_example :: UnitTest
test_example =
  runTestAuto do
    assertRight 39 =<< runStop (resumableFor bangOnly interpretStopper (interpretResumerPartial mainProgram))
    (Left (Boom "ouch") ===) =<< runStop (resumableFor bangOnly interpretStopper (interpretResumerPartialUnhandled mainProgram))
    (237 ===) =<< (resumable interpretStopper) (interpretResumer mainProgram)
