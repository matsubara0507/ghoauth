module GitHub.Login.Client
    ( GitHubLoginClient
    , baseUrl
    , mkHeader
    , LoginClient
    , newClient
    , LoginResponse
    , Error
    , Ok (..)
    ) where

import           Data.Aeson            (FromJSON (..), ToJSON (..))
import qualified Data.Aeson            as J
import           Data.Extensible
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)
import           Network.HTTP.Req      (Scheme (..), (/:), (=:))
import qualified Network.HTTP.Req      as Req
import           Network.Simple.Client (Client (..))

type ClientId = Text

newtype LoginClient = LoginClient ClientId

instance Client LoginClient where
  type ClientScheme LoginClient = 'Https
  baseUrl = const (Req.https "github.com" /: "login")
  mkHeader (LoginClient cid) = "client_id" =: cid

class Client a => GitHubLoginClient a

instance GitHubLoginClient LoginClient

newClient :: ClientId -> LoginClient
newClient = LoginClient

type LoginResponse r = Req.JsonResponse (Ok r)

type Error = Record
   '[ "error"             >: Text
    , "error_description" >: Text
    , "error_uri"         >: Text
    ]

data Ok a = Ok a | Err Error
  deriving (Show, Eq)

instance FromJSON a => FromJSON (Ok a) where
  parseJSON = J.withObject "Ok a" $ \obj ->
    if HM.member "error" obj then
      Err <$> parseJSON (J.Object obj)
    else
      Ok <$> parseJSON (J.Object obj)

instance ToJSON a => ToJSON (Ok a) where
  toJSON (Ok a)  = toJSON a
  toJSON (Err e) = toJSON e

