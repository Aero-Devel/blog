module Main where

import AccountEndpoint
import Database.PostgreSQL.Simple
import qualified Network.Wai.Handler.Warp as W
import Servant
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 6060 app
   

app :: Application
app = serve accountApi accountServer


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
