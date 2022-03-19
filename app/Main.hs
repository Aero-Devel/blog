module Main where

import Data.Function
import qualified           Network.Wai.Handler.Warp as W
import Polysemy
import AccountEndpoint
import Servant (Application)
import Servant.Server (serve, hoistServer, ServerT, Handler)


-- https://github.com/EncodePanda/todo-rest/blob/master/app/Main.hs

{-
  Skeptisk men det kan fungera.
-}
createApp :: IO Application
createApp 
  = return $ serve 
             accountApi 
             liftServer 

liftServer :: ServerT AccountAPI Handler 
liftServer = hoistServer accountApi (\sem -> sem 
                                    & runAccountStorageIO 
                                    & runM 
                                    ) accountServer
                           
main :: IO ()
main = do
  let config = Config 6969
  app <- createApp 
  W.run (port config) app

newtype Config = Config {port :: Int}