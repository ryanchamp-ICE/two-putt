module Game.Env where

data Env = Env {
    size :: (Int, Int),
    velocity :: Int,
    maxPower :: Int
}

defaultEnv :: Env
defaultEnv = Env {
    size = (9, 19),
    velocity = 1,
    maxPower = 100
}