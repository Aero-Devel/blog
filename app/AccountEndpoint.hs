{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module AccountEndpoint where
import Servant  
import qualified Data.ByteString as B
import Data.Proxy 
import Polysemy (Sem, Member, makeSem, interpret)
import AccountData

-- Define api

type AccountAPI
  =    "account" :> ReqBody '[JSON] AccountInput :> Post    '[JSON] Status -- Create / register
  :<|> "account" :> ReqBody '[JSON] AccountInput :> Get     '[JSON] Status -- Get    / login
  :<|> "account" :> ReqBody '[JSON] AccountInput :> Put     '[JSON] Status -- update pass / mail
  :<|> "account" :> ReqBody '[JSON] AccountInput :> Delete  '[JSON] Status -- Delete account

-- Connect endpoint -> funs 

data Encryptor m a where
  MakeHash :: B.ByteString -> Encryptor m B.ByteString
  Validate :: B.ByteString -> B.ByteString -> Encryptor m Bool

makeSem ''Encryptor

accountServer :: ((Member Encryptor) r , (Member AccountStorage) r)
                 => ServerT AccountAPI (Sem r)
accountServer =  register
            :<|> signIn
            :<|> updateInfo
            :<|> remove

register :: (Member AccountStorage r, Member Encryptor r) => AccountInput -> Sem r Status -- working ur here
register = undefined     
signIn = undefined
updateInfo = undefined
remove = undefined

{- -sRegister
       :<|>  llogin
       :<|>  lupdate
       :<|>  lunregister -}

-- Define AccountStorage

data AccountStorage m a where 
  InsertAcc       :: ValAcc -> AccountStorage m ()
  FindAccount     :: ValAcc -> AccountStorage m ()
  CheckAgainst    :: ValAcc -> AccountStorage m ()
  UpdateMail      :: ValAcc -> B.ByteString -> AccountStorage m ()
  UpdatePass      :: ValAcc -> B.ByteString -> AccountStorage m ()
  DeleteAcc       :: ValAcc -> AccountStorage m ()

makeSem ''AccountStorage

-- Interp AccountStorage

-- embed :: Member (Embed m) r => m a -> Sem r a

runAccountStorageIO :: Sem (AccountStorage ': r) a -> Sem r a
runAccountStorageIO = interpret $ \case
  InsertAcc    ac    -> undefined
  FindAccount  ac    -> undefined
  CheckAgainst ac    -> undefined
  UpdateMail   ac p  -> undefined
  UpdatePass   ac p  -> undefined
  DeleteAcc    ac    -> undefined


runEncryptIO :: Sem (Encryptor ': r) a -> Sem r a
runEncryptIO = interpret $ \case
  MakeHash unhashed -> undefined
  Validate try against -> undefined

-- Boilerplate -- 

accountApi :: Proxy AccountAPI
accountApi = Proxy





