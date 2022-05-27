module Game.Env where

data Env = Env {
    size :: (Int, Int),
    velocity :: Int,
    maxPower :: Int
}

defaultEnv :: Env
defaultEnv = Env {
    size = (10, 20),
    velocity = 1,
    maxPower = 100
}