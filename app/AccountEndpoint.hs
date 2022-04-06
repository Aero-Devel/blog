{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs , DeriveGeneric  #-}

module AccountEndpoint where

import AccountData as A     
import Data.Proxy
import Servant
import GHC.Generics 
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Lib

type AccountAPI 
       = "account" :> ReqBody '[JSON] RegisterForm :> Post   '[JSON] Status -- Create / register
    :<|> "account" :> ReqBody '[JSON] LoginForm    :> Get    '[JSON] Status -- Get    / login
    :<|> "account" :> ReqBody '[JSON] LoginForm    :> Put    '[JSON] Status -- update pass / mail
    :<|> "account" :> ReqBody '[JSON] LoginForm    :> Delete '[JSON] Status -- Delete account

data LoginForm = LoginForm { mE :: Maybe String , mP :: Maybe String} deriving (Show, Generic)
instance ToJSON LoginForm 
instance FromJSON LoginForm

data RegisterForm 
  = RegisterForm { mail   :: Maybe Text
                 , pass   :: Maybe Text
                 , sq     :: Maybe Text 
                 } 
                   deriving (Show, Generic)
                   
instance ToJSON RegisterForm 
instance FromJSON RegisterForm


{-
data AccountStorage m a where
  InsertAcc :: ValAcc -> AccountStorage m ()
  GetPassOf :: ValAcc -> AccountStorage m (Maybe B.ByteString)
  UpdateAccount :: ValAcc -> ValAcc -> AccountStorage m ()
  DeleteAcc :: ValAcc -> AccountStorage m ()

data Encryptor m a where
  MakeHash :: ValAcc -> Encryptor m ValAcc
  Validate :: B.ByteString -> B.ByteString -> Encryptor m Bool

-}
class (Monad m) => Persist m where
  runQuery :: (InDb a) => a -> m String
--  runQuery =  . toQuery
class InDb a where
  toQuery :: a -> String


accountServer :: ServerT AccountAPI RIO
accountServer 
    =    register
    :<|> signIn
    :<|> updateInfo
    :<|> remove

register :: RegisterForm -> RIO Status
register = undefined
remove = undefined
signIn = undefined
updateInfo = undefined

{-
register ::
  (Member AccountStorage r, Member Encryptor r) =>
  AccountInput ->
  Sem r Status -- working ur here
register acc = do
  case valAcc acc of
    (Right vAcc) -> do
      secAcc <- makeHash vAcc
      _ <- insertAcc secAcc
      return Success
    ----------------------------------------------------------
    (Left ers) -> return $ (Failure . prettyPrintErs) ers
    
    
    
    
--         ->
signIn :: (Member AccountStorage r, Member Encryptor r) => AccountInput -> Sem r Status
signIn input =
  case valAcc input of
    (Right val) ->
      case val >>= getPassOf of
        Nothing -> return wrongPassAcc
        (Just r) ->
          if validate r $ validPass val
            then return Success
            else return wrongPassAcc
    _ -> return wrongPassAcc

wrongPassAcc = Failure "Wrong account password combo"
remove :: (Member AccountStorage r, Member Encryptor r) => AccountInput -> Sem r Status
remove input =
  case valAcc input of
    isLeft -> return wrongPassAcc
    (Right val) ->
      case val >>= getPassOf  of
        Nothing -> return wrongPassAcc
        (Just r) ->
          if validate r $ validPass val
            then val >>= deleteAcc >>= return Success
            else return Failure "No such account"

-- https://github.com/EncodePanda/todo-rest/blob/master/app/Main.hs

{-
  Skeptisk men det kan fungera.
-}
-}


prettyPrintErs :: [VError] -> String
prettyPrintErs = concatMap show

-- m accIn -> accIn

accountApi :: Proxy AccountAPI
accountApi = Proxy