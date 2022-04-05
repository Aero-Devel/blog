module Main where

import AccountEndpoint
import Data.Function
import Database.PostgreSQL.Simple
import qualified Network.Wai.Handler.Warp as W
import Servant (Application)
import Servant.Server (Handler, ServerT, hoistServer, serve)

-- https://github.com/EncodePanda/todo-rest/blob/master/app/Main.hs

{-
  Skeptisk men det kan fungera.
-}
main :: IO ()
main = run 6060 $ serve accountApi

data Config = Config
  { port :: Int,
    dbInfo :: ConnectInfo
  }

db :: ConnectInfo
db =
  ConnectInfo
    { connectHost = "localhost",
      connectPort = 5438,
      connectDatabase = "postgres",
      connectPassword = "postgres",
      connectUser = "postgres"
    }
