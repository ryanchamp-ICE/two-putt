module Game.Env where

data Env = Env {
    size :: (Int, Int),
    velocity :: Int,
    maxPower :: Int,
    maxAccel :: Int
}

defaultEnv :: Env
defaultEnv = Env {
    size = (9, 19),
    velocity = 1,
    maxPower = 100,
    maxAccel = 2
}