{-# options_ghc -fplugin=Polysemy.Plugin #-}

module Polysemy.Resume.ExampleTest where

import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

import Polysemy.Resume (Stop, interpretResumableFor, resume, stop, type (!!))
import Polysemy.Resume.Interpreter.RunStop (interpretRunStop)

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
  deriving stock (Eq, Show)

newtype Blip =
  Blip { unBlip :: Int }
  deriving stock (Eq, Show)

bangOnly :: Boom -> Maybe Blip
bangOnly = \case
  Bang n -> Just (Blip n)
  Boom _ -> Nothing

handleStopper ::
  Member (Stop Boom) r =>
  Stopper z a -> Sem r a
handleStopper = \case
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
  runTestAuto $ interpretRunStop do
    assertRight 39 =<< runError (interpretResumableFor bangOnly handleStopper (interpretResumerPartial mainProgram))
    (Left (Boom "ouch") ===) =<< runError (interpretResumableFor bangOnly handleStopper (interpretResumerPartialUnhandled mainProgram))
