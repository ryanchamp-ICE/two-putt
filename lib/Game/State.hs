module Game.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.Type (Game (..), TileType(..))
import Data.List (find)

data Direction = Positive | Negative | Neutral
    deriving (Show, Eq)
data GameState = Menu | Game | LoadHole
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
    holeTiles :: [[TileType]],
    totalScore :: Int
}

defaultState :: State
defaultState = State {
    gameState = Menu,
    playerState = Stroke,
    ballPosition = (9, 19),
    ballDirection = (Negative, Negative),
    strokeDirection = (Neutral, Neutral),
    strokePower = 1,
    strokeNumber = 1,
    holeNumber = 1,
    holePosition = (0, 0),
    holeTiles = [[]],
    totalScore = 0
}

mapFile :: String
mapFile = "55555655555555555555\n55555655555555555555\n55055655555555555555\n55555655555555555555\n22233355555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n"

createTileType :: Char -> (Int, Int) -> TileType
createTileType '0' pos = Hole pos
createTileType '1' pos = Southwest pos
createTileType '2' pos = South pos
createTileType '3' pos = Southeast pos
createTileType '4' pos = West pos
createTileType '5' pos = Flat pos
createTileType '6' pos = East pos
createTileType '7' pos = Northwest pos
createTileType '8' pos = North pos
createTileType '9' pos = Northeast pos
createTileType _ _ = Unknown

createTileLines :: String -> Int -> [TileType]
createTileLines tileString lineNumber = zipWith (\i ch -> createTileType ch (lineNumber, i)) [0..] tileString

parseMap :: [String] -> [[TileType]]
parseMap tileFile = zipWith (\i str -> createTileLines str i) [0..] tileFile

findHolePosition :: [[TileType]] -> (Int, Int)
findHolePosition tileLines = undefined

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
    holeTiles
    totalScore) = 
        case prevGameState of
            Menu -> State {
                gameState = LoadHole,
                playerState = Aim,
                ballPosition = (prevBallX, prevBallY),
                ballDirection = (prevXDir, prevYDir),
                strokeDirection = strokeDirection,
                strokePower = 1,
                strokeNumber = 1,
                holeNumber = 1,
                holePosition = holePosition,
                holeTiles = holeTiles,
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
                    holeTiles = holeTiles,
                    totalScore = totalScore
                }
            LoadHole -> State {
                gameState = Game,
                playerState = Stroke,
                ballPosition = (prevBallX, prevBallY),
                ballDirection = (prevXDir, prevYDir),
                strokeDirection = strokeDirection,
                strokePower = 1,
                strokeNumber = 1,
                holeNumber = newHoleNumber,
                holePosition = newHolePosition,
                holeTiles = parseMap (lines mapFile),
                totalScore = totalScore
            }
    where
        newHoleNumber = holeNumber
        newHolePosition = findHolePosition holeTiles
        newStrokeNumber = strokeNumber
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
