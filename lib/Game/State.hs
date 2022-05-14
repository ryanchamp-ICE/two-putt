module Game.State where

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
    ballDirection :: (Direction, Direction)
}

defaultState :: State
defaultState = State {
    gameState = Menu,
    ballPosition = (0, 0),
    ballDirection = (Neutral, Neutral)
}