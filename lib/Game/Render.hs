module Game.Render where

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

renderIO :: IO ()
renderIO = do
    putStrLn $ renderTiles mapLines (5, 9)
    where
        mapLines = parseMap (lines mapFile)