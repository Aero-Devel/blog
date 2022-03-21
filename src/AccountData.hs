{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module AccountData (
  VError,
  ValAcc(..),
  valAcc,
  AccountInput,
  Status(Success,Failure)
) where

import qualified Data.ByteString as B
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)
import Database.PostgreSQL.Simple

valAcc :: AccountInput -> Either [VError] ValAcc 
valAcc = undefined

data ValAcc = 
  ValAcc { validMail  :: B.ByteString
         , validPass :: B.ByteString
         } deriving (Eq, Generic, ToRow, FromRow, Show)

data AccountInput 
  = AccountInput { mail     :: String
                 , password :: String
                 } deriving (Eq, Generic, ToRow, FromRow)

instance ToJSON AccountInput
instance FromJSON AccountInput

data Status 
  = Success  
  | Failure String -- reason(s)
 deriving (Eq, Generic, Show)

instance Semigroup Status where
  (<>) (Failure f) (Failure f2) = Failure (f ++ "," ++ f2)
  (<>) f1 Success = f1
  (<>) Success f2  = f2

instance Monoid Status where
  mempty = Success

instance ToJSON Status
instance FromJSON Status

data VError = InvalidEmail | InvalidPassword deriving Show

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
  ans :: [ValAcc] <- query con getAccountSQL (validMail acc, validPass acc)
  return ans
    

deleteAccount :: Query
deleteAccount = "DELETE FROM accounts WHERE email=? AND hashedPassword=?"

executeDeleteAccount :: ValAcc -> ConnectInfo -> IO ()
executeDeleteAccount
    acc conInfo = do 
    c <- connect conInfo
    execute c deleteAccount (validMail acc,validPass acc)
    return ()
    

updateAccountEmailSQL :: Query
updateAccountEmailSQL = "UPDATE accounts SET email=? WHERE email=? AND hashedPassword=?"


executeUpdateEmailAccount :: ValAcc -> B.ByteString -> ConnectInfo -> IO () 
executeUpdateEmailAccount 
  acc new con = do 
    c <- connect con
    _ <- execute c updateAccountEmailSQL (new ,validMail acc,validPass acc)
    return ()


updateAccountPassSQL :: Query
updateAccountPassSQL = "UPDATE accounts SET hashedPassword=? WHERE email=? AND hashedPassword=?"


executeUpdatePassAccount :: ValAcc -> B.ByteString -> ConnectInfo -> IO () 
executeUpdatePassAccount 
  acc new con = do 
    c <- connect con
    _ <- execute c updateAccountEmailSQL (new, validMail acc,validPass acc)
    return ()
    

createAccountSQL :: Query
createAccountSQL =  "INSERT INTO accounts(email, password) VALUES (?, ?)"

--executeCreateAccount = execute conInfo createAccountSQL 

executeCreateAccount :: ValAcc -> ConnectInfo -> IO ()
executeCreateAccount 
 acc con = do 
    c <- connect con
    execute c createAccountSQL (validMail acc,validPass acc)
    return ()
