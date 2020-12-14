module Polysemy.Resume.ExampleTest where

import Polysemy (interpret, makeSem)
import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

import Polysemy.Error (runError)
import Polysemy.Resume (Stop, resumable, resumableFor, resume, stop, type (!!))

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
  deriving (Eq, Show)

newtype Blip =
  Blip { unBlip :: Int }
  deriving (Eq, Show)

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
  Member (Stopper !! Blip) r =>
  InterpreterFor Resumer r
interpretResumerPartialUnhandled =
  interpret \ MainProgram ->
    resume (192 <$ stopBoom) \ (Blip num) ->
      pure (num * 3)

interpretResumerPartial ::
  Member (Stopper !! Blip) r =>
  InterpreterFor Resumer r
interpretResumerPartial =
  interpret \ MainProgram ->
    resume (192 <$ stopBang) \ (Blip num) ->
      pure (num * 3)

interpretResumer ::
  Member (Stopper !! Boom) r =>
  InterpreterFor Resumer r
interpretResumer =
  interpret \ MainProgram ->
    resume (192 <$ stopBang) \ _ ->
      pure 237

test_example :: UnitTest
test_example =
  runTestAuto do
    assertRight 39 =<< runError (resumableFor bangOnly interpretStopper (interpretResumerPartial mainProgram))
    (Left (Boom "ouch") ===) =<< runError (resumableFor bangOnly interpretStopper (interpretResumerPartialUnhandled mainProgram))
    (237 ===) =<< (resumable interpretStopper) (interpretResumer mainProgram)
