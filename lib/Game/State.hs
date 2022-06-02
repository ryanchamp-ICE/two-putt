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
intToDirection (-1) = Negative
intToDirection 0 = Neutral
intToDirection 1 = Positive
intToDirection _ = error "Invalid direction"

directionToInt :: Direction -> Int
directionToInt Negative = (-1)
directionToInt Neutral = 0
directionToInt Positive = 1

tileToDirection :: TileType -> (Direction, Direction)
tileToDirection (Southwest _) = (Positive, Negative)
tileToDirection (South _) = (Positive, Neutral)
tileToDirection (Southeast _) = (Positive, Positive)
tileToDirection (East _) = (Neutral, Positive)
tileToDirection (Northeast _) = (Negative, Positive)
tileToDirection (North _) = (Negative, Neutral)
tileToDirection (Northwest _) = (Negative, Negative)
tileToDirection (West _) = (Neutral, Negative)  
tileToDirection _ = (Neutral, Neutral)

collideDirection :: Direction -> Direction -> Direction
collideDirection Positive Positive = Positive
collideDirection Positive Negative = Neutral
collideDirection Positive Neutral = Positive
collideDirection Negative Positive = Neutral
collideDirection Negative Negative = Negative
collideDirection Negative Neutral = Negative
collideDirection Neutral _ = Neutral

collideDirections :: (Direction, Direction) -> (Direction, Direction) -> (Direction, Direction)
collideDirections (ballY, ballX) (tileY, tileX) = (collideDirection ballY tileY, collideDirection ballX tileX)

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

isHole :: TileType -> Bool
isHole (Hole _) = True
isHole _ = False

isTilePosition :: (Int, Int) -> TileType -> Bool
isTilePosition pos (Hole pos') = pos == pos'
isTilePosition pos (Southwest pos') = pos == pos'
isTilePosition pos (South pos') = pos == pos'
isTilePosition pos (Southeast pos') = pos == pos'
isTilePosition pos (West pos') = pos == pos'
isTilePosition pos (Flat pos') = pos == pos'
isTilePosition pos (East pos') = pos == pos'
isTilePosition pos (Northwest pos') = pos == pos'
isTilePosition pos (North pos') = pos == pos'
isTilePosition pos (Northeast pos') = pos == pos'
isTilePosition _ _ = False

findTileByPosition :: (Int, Int) -> [[TileType]] -> TileType
findTileByPosition pos tiles = 
    case find (isTilePosition pos) $ concat tiles of
        Nothing -> error "Tile not found"
        Just tile -> tile

findHolePosition :: [[TileType]] -> (Int, Int)
findHolePosition tileLines =
    case find (isHole) $ concat tileLines of
        Nothing -> error "No hole found"
        Just (Hole pos) -> pos

tileDirectionByPosition :: (Int, Int) -> [[TileType]] -> (Direction, Direction)
tileDirectionByPosition pos tiles = tileToDirection $ findTileByPosition pos tiles

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
                    strokePower = newStrokePower,
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
                strokePower = 50,
                strokeNumber = 1,
                holeNumber = newHoleNumber,
                holePosition = newHolePosition,
                holeTiles = newHoleTiles,
                totalScore = totalScore
            }
    where
        newHoleNumber = holeNumber
        newHoleTiles = parseMap (lines mapFile)
        newHolePosition = findHolePosition newHoleTiles
        newStrokeNumber = strokeNumber
        newStrokePower = max (strokePower - (abs velocity)) 0
        (tileXDir, tileYDir) = tileToDirection $ findTileByPosition (prevBallX, prevBallY) holeTiles
        newXDirUnbounded = collideDirection prevXDir tileXDir
        newYDirUnbounded = collideDirection prevYDir tileYDir
        newXUnbounded = prevBallX + directionToInt newXDirUnbounded * (min newStrokePower velocity)
        newYUnbounded = prevBallY + directionToInt newYDirUnbounded * (min newStrokePower velocity)
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
            case newXDirUnbounded of
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
            case newYDirUnbounded of
                Neutral -> Neutral
                Positive ->
                    if newYUnbounded > height
                    then Negative
                    else Positive
                Negative ->
                    if newYUnbounded < 0
                    then Positive
                    else Negative
