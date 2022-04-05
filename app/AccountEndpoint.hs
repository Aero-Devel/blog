{-# LANGUAGE TypeOperators #-}
module AccountEndpoint where

import AccountData as A     
import qualified Data.ByteString as B
import Data.Proxy
import Servant




type AccountAPI 
       = "account" :> ReqBody '[JSON] AccountInput :> Post   '[JSON] Status -- Create / register
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Get    '[JSON] Status -- Get    / login
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Put    '[JSON] Status -- update pass / mail
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Delete '[JSON] Status -- Delete account
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

class InDb a where
  toQuery :: a -> String

data DbAction = Select | Insert | Delete | Update
data Selector = AllMatching | All | NoneMatching
data DbQuery = DbAction ValAcc Selector




accountServer 
    =    register
    :<|> signIn
    :<|> updateInfo
    :<|> remove

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