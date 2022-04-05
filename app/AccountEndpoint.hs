{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module AccountEndpoint where

import AccountData as A     
import qualified Data.ByteString as B
import Data.Proxy
import Servant




type AccountAPI =
  "account" :> ReqBody '[JSON] AccountInput :> Post '[JSON] Status -- Create / register
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Get '[JSON] Status -- Get    / login
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Put '[JSON] Status -- update pass / mail
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
  select :: (InDb a) => a -> m Text
  

data DbAction = Select | Insert | Delete 
data Selector = AllMatching | All | NoneMatching

data DbQuery = DbAction ValAcc Selector



makeSem ''Encryptor

accountServer 
    =    register
    :<|> signIn
    :<|> updateInfo
    :<|> remove


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
-}


prettyPrintErs :: [VError] -> String
prettyPrintErs = concatMap show

-- m accIn -> accIn
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


accountApi :: Proxy AccountAPI
accountApi = Proxy