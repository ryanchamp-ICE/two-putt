module Game.Render where

import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.State (State (..), GameState (Menu, Game, LoadHole))
import Game.Type (Game, TileType(..))

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

renderTileLines :: [TileType] -> (Int, Int) -> String
renderTileLines xs ballPos = "|" ++ map (`renderTile` ballPos) xs ++ "|\n"

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

renderDebugInfo :: State -> String
renderDebugInfo state = "Game State: " ++ show (gameState state) ++ " | " ++
                        "Player State: " ++ show (playerState state) ++ "\n"


renderPowerBar :: Env -> State -> String
renderPowerBar env state = "Stroke Power: " ++ show (strokePower state) ++ " | " ++
                           "Stroke Max: " ++ show (maxPower env) ++ "\n"

renderGame :: Env -> State -> String
renderGame env state = renderTiles (holeTiles state) (ballPosition state) ++ "\n" ++
                       renderPowerBar env state ++
                       renderGameInfo state ++
                       renderDebugInfo state

renderString :: Game Env State String
renderString = do
    env <- ask
    state <- lift get
    case gameState state of
        Menu -> return "Welcome to Two Putt!"
        Game -> return (renderGame env state)
        LoadHole -> return "Loading hole..."

renderIO :: Game Env State ()
renderIO = do
    output <- renderString
    lift $ lift (putStrLn output)
    