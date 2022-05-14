module Game.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

type Game env state a = ReaderT env (StateT state IO) a

runGame :: env -> state -> Game env state a -> IO a
runGame env state action = evalStateT (runReaderT action env) state