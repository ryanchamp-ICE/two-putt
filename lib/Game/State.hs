module Game.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.Type (Game (..))

data Direction = Positive | Negative | Neutral
    deriving (Show, Eq)
data GameState = Menu | Game
    deriving (Show, Eq)
data PlayerState = Aim | SetPower | Stroke
    deriving (Show, Eq)

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
    playerState :: PlayerState,
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
    playerState = Stroke,
    ballPosition = (9, 19),
    ballDirection = (Negative, Negative),
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
nextInternal (Env (width, height) velocity maxPower) prevState@(State 
    prevGameState
    prevPlayerState 
    (prevBallX, prevBallY) 
    (prevXDir, prevYDir) 
    strokeDirection 
    strokePower
    strokeNumber
    holeNumber 
    holePosition 
    totalScore) = 
        case prevGameState of
            Menu -> State {
                gameState = Menu,
                playerState = Aim,
                ballPosition = (prevBallX, prevBallY),
                ballDirection = (prevXDir, prevYDir),
                strokeDirection = strokeDirection,
                strokePower = 1,
                strokeNumber = 1,
                holeNumber = 1,
                holePosition = holePosition,
                totalScore = totalScore
            }
            Game -> case prevPlayerState of
                Aim -> undefined
                SetPower -> undefined
                Stroke -> State {
                    gameState = Game,
                    playerState = prevPlayerState,
                    ballPosition = (newX, newY),
                    ballDirection = (newXDir, newYDir),
                    strokeDirection = strokeDirection,
                    strokePower = 1,
                    strokeNumber = newStrokeNumber,
                    holeNumber = newHoleNumber,
                    holePosition = holePosition,
                    totalScore = totalScore
                }
    where
        newStrokeNumber = strokeNumber
        newHoleNumber = holeNumber
        newXUnbounded = prevBallX + directionToInt prevXDir * velocity
        newYUnbounded = prevBallY + directionToInt prevYDir * velocity
        newX =
            case prevXDir of
                Neutral -> newXUnbounded
                Positive -> min newXUnbounded width
                Negative -> max newXUnbounded 0
        newY =
            case prevYDir of
                Neutral -> newYUnbounded
                Positive -> min newYUnbounded height
                Negative -> max newYUnbounded 0
        newXDir =
            case prevXDir of
                Neutral -> Neutral
                Positive ->
                    if newXUnbounded > width
                    then Negative
                    else Positive
                Negative ->
                    if newXUnbounded < 0
                    then Positive
                    else Negative
        newYDir =
            case prevYDir of
                Neutral -> Neutral
                Positive ->
                    if newYUnbounded > height
                    then Negative
                    else Positive
                Negative ->
                    if newYUnbounded < 0
                    then Positive
                    else Negative
