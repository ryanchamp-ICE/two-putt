module Game.Env where

data Env = Env {
    velocity :: Int,
    maxPower :: Int
}

defaultEnv :: Env
defaultEnv = Env {
    velocity = 1,
    maxPower = 100
}