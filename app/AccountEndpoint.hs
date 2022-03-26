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
import Polysemy (Member, Sem, interpret, makeSem, embed)
import Servant

-- Define api

my_foldr :: (a -> b -> b) -> b -> [a] -> b
my_foldr = 



type AccountAPI =
  "account" :> ReqBody '[JSON] AccountInput :> Post '[JSON] Status -- Create / register
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Get '[JSON] Status -- Get    / login
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Put '[JSON] Status -- update pass / mail
    :<|> "account" :> ReqBody '[JSON] AccountInput :> Delete '[JSON] Status -- Delete account

-- Connect endpoint -> funs

data AccountStorage m a where
  InsertAcc :: ValAcc -> AccountStorage m ()
  GetPassOf :: ValAcc -> AccountStorage m (Maybe B.ByteString)
  UpdateAccount :: ValAcc -> ValAcc -> AccountStorage m ()
  DeleteAcc :: ValAcc -> AccountStorage m ()

makeSem ''AccountStorage

data Encryptor m a where
  MakeHash :: ValAcc -> Encryptor m ValAcc
  Validate :: B.ByteString -> B.ByteString -> Encryptor m Bool

makeSem ''Encryptor

accountServer ::
  ((Member Encryptor) r, (Member AccountStorage) r) =>
  ServerT AccountAPI (Sem r)
accountServer =
  register
    :<|> signIn
    :<|> updateInfo
    :<|> remove

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

updateInfo :: (Member AccountStorage r, Member Encryptor r) => AccountInput -> Sem r Status
updateInfo input =
  undefined

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

{- -sRegister
       :<|>  llogin
       :<|>  lupdate
       :<|>  lunregister -}

-- Define AccountStorage
-- Interp AccountStorage
-- embed :: Member (Embed m) r => m a -> Sem r a

runAccountStorageIO :: Sem (AccountStorage ': r) a -> Sem r a
runAccountStorageIO = interpret $ \case
  InsertAcc ac -> embed $ executeCreateAccount ac db
  GetPassOf mail -> undefined
  UpdateAccount orig new -> undefined
  DeleteAcc ac -> undefined


runEncryptIO :: Sem (Encryptor ': r) a -> Sem r a
runEncryptIO = interpret $ \case
  MakeHash raw -> undefined
  Validate try against -> undefined

-- Boilerplate --

accountApi :: Proxy AccountAPI
accountApi = Proxy

{-
  https://github.com/NixOS/nix/issues/4356
  sudo chown -R ihsan:staff /nix
  DOCS
  https://buildmedia.readthedocs.org/media/pdf/reflex-frp/latest/reflex-frp.pdf
  LINK2LINKS
  https://reflex-frp.org/resources
-}
