module Game.Env where

data Env = Env {
    velocity :: Int
}

defaultEnv :: Env
defaultEnv = Env {
    velocity = 1
}