module Game.Render where

import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Class (lift)

import Game.Env (Env (..))
import Game.State (State (..), GameState (Menu, Game))
import Game.Type (Game)

mapFile :: String
mapFile = "1555O555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n55555555555555555555\n"

data TileType
    = Flat (Int, Int)
    | SWSlope (Int, Int)
    | Hole (Int, Int)
    | Unknown
    deriving Show

createTileType :: Char -> (Int, Int) -> TileType
createTileType '5' pos = Flat pos
createTileType 'O' pos = Hole pos
createTileType '1' pos = SWSlope pos
createTileType _ _ = Unknown

renderTileCharacter :: (Int, Int) -> (Int, Int) -> Char -> Char
renderTileCharacter (tileX, tileY) (ballX, ballY) ch
    | tileX == ballX && tileY == ballY = '*'
    | otherwise = ch

renderTile :: TileType -> (Int, Int) -> Char
renderTile (Flat tilePos) ballPos = renderTileCharacter tilePos ballPos ' '
renderTile (Hole tilePos) ballPos = renderTileCharacter tilePos ballPos 'O'
renderTile (SWSlope tilePos) ballPos = renderTileCharacter tilePos ballPos '\x2199'
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

renderGame :: Env -> State -> String
renderGame env state = renderTiles mapLines (ballPosition state) ++ "\n" ++
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
    