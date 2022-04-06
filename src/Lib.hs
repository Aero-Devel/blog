{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    , RIO(..)
    , AppContext (..)
    , ntFeedCtx
    ) where

import Control.Monad.Reader
import Servant (Handler)
import Data.Text (Text)
import Database.PostgreSQL.Simple
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Monadstack

newtype RIO a 
  = RIO { unRIO :: ReaderT AppContext IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader AppContext)

data AppContext = AppContext { port  :: Int
                             , dbCon :: ConnectInfo}

-- Want to incorporate stack into servant

runRIO :: RIO a -> AppContext -> IO a
runRIO = runReaderT . unRIO

db :: ConnectInfo
db =
  ConnectInfo
    { connectHost     = "localhost" ,
      connectPort     = 5438        ,
      connectDatabase = "postgres"  ,
      connectPassword = "postgres"  ,
      connectUser     = "postgres"
    }


-- https://magnus.therning.org/2021-03-19-custom-monad-with-servant-and-throwing-errors.html

ntFeedCtx :: AppContext -> RIO a -> Handler a
ntFeedCtx ctx rio = liftIO $ runRIO rio ctx



