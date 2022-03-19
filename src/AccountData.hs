{-# LANGUAGE DeriveGeneric #-}

module AccountData (
  VError,
  ValAcc(..),
  AccountInput,
  Status
) where

import qualified Data.ByteString as B
import GHC.Generics (Generic)
import Data.Aeson.Types (ToJSON, FromJSON)

valAcc :: AccountInput -> Either VError ValAcc 
valAcc = undefined

data ValAcc = 
  ValAcc { validMail  :: B.ByteString
         , hashedPass :: B.ByteString
         }

data AccountInput 
  = AccountInput { mail     :: String
                 , password :: String
                 } deriving (Eq, Generic)

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

{-====================
      DATABASE
====================-}

