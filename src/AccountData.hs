{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AccountData (
  VError,
  ValAcc(..),
  AccountInput,
  Status
) where

import qualified Data.ByteString as B
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.PostgreSQL.Simple

valAcc :: AccountInput -> Either VError ValAcc 
valAcc = undefined

data ValAcc = 
  ValAcc { validMail  :: B.ByteString
         , hashedPass :: B.ByteString
         } deriving (Eq, Generic, ToRow, FromRow, Show)

data AccountInput 
  = AccountInput { mail     :: String
                 , password :: String
                 } deriving (Eq, Generic, ToRow, FromRow)

instance ToJSON AccountInput
instance FromJSON AccountInput

data Status 
  = Success  
  | Failure Reason -- reason(s)
 deriving (Eq, Generic)
type Reason = String

instance ToJSON Status
instance FromJSON Status

data VError = InvalidEmail | InvalidPassword

getAccountSQL :: Query
getAccountSQL = "SELECT * FROM accounts WHERE email=?"

getAccEmailPass :: Query
getAccEmailPass = "SELECT * FROM accounts WHERE email=? AND hashedPassword=?"

getAccountWithEmail :: ValAcc -> ConnectInfo -> IO [ValAcc]
getAccountWithEmail acc con
  = do 
    c <- connect con
    ans :: [ValAcc] <- query c getAccountSQL $ (Only . validMail) acc
    return ans
    
getAccountMatchingAll :: ValAcc -> ConnectInfo -> IO [ValAcc]
getAccountMatchingAll acc c = do 
  con <- connect c
  ans :: [ValAcc] <- query con getAccountSQL (validMail acc, hashedPass acc)
  return ans
    

deleteAccount :: Query
deleteAccount = "DELETE FROM accounts WHERE email=? AND hashedPassword=?"

executeDeleteAccount :: ValAcc -> ConnectInfo -> IO ()
executeDeleteAccount
    acc conInfo = do 
    c <- connect conInfo
    execute c deleteAccount (validMail acc,hashedPass acc)
    return ()
    

updateAccountEmailSQL :: Query
updateAccountEmailSQL = "UPDATE accounts SET email=? WHERE email=? AND hashedPassword=?"

executeUpdateEmailAccount :: ValAcc -> B.ByteString -> ConnectInfo -> IO () 
executeUpdateEmailAccount 
  acc new con = do 
    c <- connect con
    _ <- execute c updateAccountEmailSQL (new ,validMail acc,hashedPass acc)
    return ()

updateAccountPassSQL :: Query
updateAccountPassSQL = "UPDATE accounts SET hashedPassword=? WHERE email=? AND hashedPassword=?"

executeUpdatePassAccount :: ValAcc -> B.ByteString -> ConnectInfo -> IO () 
executeUpdatePassAccount 
  acc new con = do 
    c <- connect con
    _ <- execute c updateAccountEmailSQL (new, validMail acc,hashedPass acc)
    return ()
    

