module Game.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.Type (Game (..))

data Direction = Positive | Negative | Neutral
data GameState = Menu | Game 

intToDirection :: Int -> Direction
intToDirection -1 = Negative
intToDirection 0 = Neutral
intToDirection 1 = Positive
intToDirection _ = error "Invalid direction"

directionToInt :: Direction -> Int
directionToInt Negative = -1
directionToInt Neutral = 0
directionToInt Positive = 1

data State = State {
    gameState :: GameState,
    ballPosition :: (Int, Int),
    ballDirection :: (Direction, Direction),
    strokeDirection :: (Direction, Direction),
    strokePower :: Int,
    strokeNumber :: Int,
    holeNumber :: Int,
    holePosition :: (Int, Int),
    totalScore :: Int
}

defaultState :: State
defaultState = State {
    gameState = Game,
    ballPosition = (9, 19),
    ballDirection = (Neutral, Neutral),
    strokeDirection = (Neutral, Neutral),
    strokePower = 1,
    strokeNumber = 1,
    holeNumber = 1,
    holePosition = (0, 0),
    totalScore = 0
}

next :: Game Env State ()
next = do
    env <- ask
    prevState <- lift get
    lift (put $ nextInternal env prevState)

nextInternal :: Env -> State -> State
nextInternal (Env velocity maxPower) prevState = undefined