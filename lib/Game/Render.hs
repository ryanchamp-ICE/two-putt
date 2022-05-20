module Game.Render where

import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.State (State (..), GameState (Menu, Game))
import Game.Type (Game)

mapFile :: String
mapFile = "55555655555555555555\n55555655555555555555\n55055655555555555555\n55555655555555555555\n22233355555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n"

data TileType
    = Hole (Int, Int)
    | Southwest (Int, Int)
    | South (Int, Int)
    | Southeast (Int, Int)
    | West (Int, Int)
    | Flat (Int, Int)
    | East (Int, Int)
    | Northwest (Int, Int)
    | North (Int, Int)
    | Northeast (Int, Int)
    | Unknown
    deriving Show

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

renderTileCharacter :: (Int, Int) -> (Int, Int) -> Char -> Char
renderTileCharacter (tileX, tileY) (ballX, ballY) ch
    | tileX == ballX && tileY == ballY = '*'
    | otherwise = ch

renderTile :: TileType -> (Int, Int) -> Char
renderTile (Hole tilePos) ballPos = renderTileCharacter tilePos ballPos 'O'
renderTile (Southwest tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2199'
renderTile (South tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2193'
renderTile (Southeast tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2198'
renderTile (West tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2190'
renderTile (Flat tilePos) ballPos = renderTileCharacter tilePos ballPos ' '
renderTile (East tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2192'
renderTile (Northwest tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2196'
renderTile (North tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2191'
renderTile (Northeast tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2197'
renderTile Unknown _ = error "Unknown tile type"

createTileLines :: String -> Int -> [TileType]
createTileLines tileString lineNumber = zipWith (\i ch -> createTileType ch (lineNumber, i)) [0..] tileString

renderTileLines :: [TileType] -> (Int, Int) -> String
renderTileLines xs ballPos = "|" ++ map (`renderTile` ballPos) xs ++ "|\n"

parseMap :: [String] -> [[TileType]]
parseMap tileFile = zipWith (\i str -> createTileLines str i) [0..] tileFile

renderTiles :: [[TileType]] -> (Int, Int) -> String
renderTiles xs ballPos = concatMap (`renderTileLines` ballPos) xs

renderPosition :: (Int, Int) -> String
renderPosition (x, y) = "(" ++ show x ++ ", " ++ show y ++ ")"

renderGameInfo :: State -> String
renderGameInfo state = "Ball Pos: " ++ renderPosition (ballPosition state) ++ " | " ++
                       "Hole Pos: " ++ renderPosition (holePosition state) ++ " | " ++
                       "Hole Number: " ++ show (holeNumber state) ++ " | " ++
                       "Stroke Number: " ++ show (strokeNumber state) ++ " | " ++
                       "Total Score: " ++ show (totalScore state) ++ "\n"

renderPowerBar :: Env -> State -> String
renderPowerBar env state = "Stroke Power: " ++ show (strokePower state) ++ " | " ++
                           "Stroke Max: " ++ show (maxPower env) ++ "\n"

renderGame :: Env -> State -> String
renderGame env state = renderTiles mapLines (ballPosition state) ++ "\n" ++
                       renderPowerBar env state ++
                       renderGameInfo state
    where
        mapLines = parseMap (lines mapFile)

renderString :: Game Env State String
renderString = do
    env <- ask
    state <- lift get
    case gameState state of
        Menu -> return "Welcome to Two Putt!"
        Game -> return (renderGame env state)

renderIO :: Game Env State ()
renderIO = do
    output <- renderString
    lift $ lift (putStrLn output)
    