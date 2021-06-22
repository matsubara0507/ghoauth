module GitHub.Login.Test.Client
    ( TestClient(..)
    ) where

import           GitHub.Login.Client (GitHubLoginClient)
import           Network.HTTP.Req    (Scheme (Http), http, port, (/:))
import           Network.Simple      (Client (..))

data TestClient = TestClient

instance Client TestClient where
  type ClientScheme TestClient = 'Http
  baseUrl _ = http "localhost" /: "api"
  mkHeader _ = mconcat [ port 8000 ]

instance GitHubLoginClient TestClient
