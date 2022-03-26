
module CreateAccount where
{-
import AccountData (VError)

import Data.Maybe
import Polysemy
import Data.ByteString as B
import Data.ByteString.Char8
import Polysemy.Input
import Polysemy.Output
import Data.ByteString.UTF8 as BSU
import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string

newtype Password = Password {unPassword :: B.ByteString}
newtype Hash = Hash B.ByteString
newtype Email = Email B.ByteString
newtype Response = Response B.ByteString
newtype Cookie = Cookie B.ByteString
data VAcc = VAcc Email Hash

  -- The m here states were in a monad (IO on live)
data Hasher m a where 
  -- making hash requires us to take some plaintext and turn into password
  MkHash   :: B.ByteString -> Hasher m Hash
  -- validate requires a hash and the original bytestring
  Validate :: B.ByteString -> Hash -> Hasher m Bool

makeSem ''Hasher


data AccountStorage m a where 
  NewAccount    :: VAcc ->          AccountStorage m ( Either [VError] () )
  Login         :: VAcc ->          AccountStorage m ( Either [VError] Cookie )
  AlterEmail    :: VAcc -> VAcc ->  AccountStorage m ( Either [VError] () )
  DeleteAccount :: VAcc ->          AccountStorage m ( Either [VError] () )

makeSem ''AccountStorage

mkVAcc :: (Monad m) => Email -> Password -> m (Either [VError] VAcc)
mkVAcc = undefined



{-
createAccount :: Maybe String -> Maybe String -> a
createAccount mail pass
  = do eRes <- valFormat mail pass
       res  <- (register . scLeft ) hashPass eRes
       return res

valFormat :: Maybe String -> Maybe String -> Either [VError] ValidRegInfo
valFormat (usr) (pass) = undefined

scLeft :: Either a b -> (x -> fx) -> Either a fx 
scLeft =  undefined

register :: Account -> Either [] b
register = undefined

respondErrs :: [VError] -> Response
respondErrs = undefined

respondSucc :: Account -> Response
respondSucc = undefined

hashPass :: ValidRegInfo -> IO Account
hashPass = undefined

-}
--}