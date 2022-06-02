module Game.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)
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

type Game env state a = ReaderT env (StateT state IO) a

runGame :: env -> state -> Game env state a -> IO a
runGame env state action = evalStateT (runReaderT action env) state