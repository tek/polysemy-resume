module Polysemy.Resume.Data.Stop where

data Stop e :: Effect where
  Stop :: e -> Stop e m a

makeSem ''Stop
