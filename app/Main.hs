module Main where

import AccountEndpoint
import Control.Monad.Reader (runReader)
import Database.PostgreSQL.Simple
import Lib
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Handler.Warp as W
import Servant

-- det här måste jag läsa om
-- https://docs.servant.dev/en/stable/tutorial/Server.html#the-handler-monad

main :: IO ()
main = run 6060 app

app :: Application
app = serve accountApi readerServer

readerServer = hoistServer accountApi nt accountServer


-- reader -> handler
nt :: RIO a -> Handler a
nt =
  ntFeedCtx $
    AppContext
      { port = 6060,
        dbCon =
          ConnectInfo
            { connectHost     = "localhost"
            , connectPort     =  5438
            , connectDatabase = "postgres"
            , connectPassword = "postgres"
            , connectUser     = "postgres"
            }
      }
