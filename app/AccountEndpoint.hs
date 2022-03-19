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

accountServer :: Member AccountStorage r => ServerT AccountAPI (Sem r)
accountServer = undefined 

{- -sRegister
       :<|>  llogin
       :<|>  lupdate
       :<|>  lunregister -}

-- Define AccountStorage

data AccountStorage m a where 
  Register     :: ValAcc -> AccountStorage m ()
  Login        :: ValAcc -> AccountStorage m ()
  Update       :: ValAcc -> AccountStorage m ()
  Unregister   :: ValAcc -> AccountStorage m ()

makeSem ''AccountStorage

-- Interp AccountStorage

-- embed :: Member (Embed m) r => m a -> Sem r a

runAccountStorageIO :: Sem (AccountStorage ': r) a -> Sem r a
runAccountStorageIO = interpret $ \case
  Register   valAcc -> undefined -- embed $
  Login      valAcc -> undefined
  Update     valAcc -> undefined
  Unregister valAcc -> undefined


-- Boilerplate -- 

accountApi :: Proxy AccountAPI
accountApi = Proxy





