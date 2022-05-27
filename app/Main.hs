module Main where

import Game 
    ( Game,
      Env (..),
      State (..),
      defaultEnv,
      defaultState,
      renderIO,
      next,
      runGame )

import Control.Concurrent (threadDelay)

import Control.Monad.Trans.Class (lift)

run :: Game Env State ()
run = do
  renderIO
  next
  lift $ lift $ threadDelay 1000000
  run

main :: IO ()
main = runGame defaultEnv defaultState run
