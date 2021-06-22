module GitHub.Login.Test.MockServer where

import           Control.Concurrent
import           Data.Extensible
import qualified GitHub.Login             as GitHub
import qualified GitHub.Login.Device      as GitHub
import           GitHub.Login.Test.Helper (returnJsonFile)
import           Network.Wai.Handler.Warp
import           Servant

type API = "api" :> (DeviceCodeAPI :<|> AccessTokenAPI)

type DeviceCodeAPI = "device" :> "code" :> Post '[JSON] (GitHub.Ok GitHub.LoginCode)

type AccessTokenAPI = "oauth" :> "access_token" :> Post '[JSON] (GitHub.Ok GitHub.AccessTokenInfo)

api :: Proxy API
api = Proxy

server :: Server API
server = requestDeviceCode :<|> requestAccessToken
  where
    requestDeviceCode = returnJsonFile "test/fixture/code.json"
    requestAccessToken = returnJsonFile "test/fixture/token.json"

mockServer :: IO ()
mockServer = run 8000 (serve api server)

runMockServer :: IO () -> IO ()
runMockServer action = do
  _ <- forkIO mockServer
  action


