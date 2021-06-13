module GitHub.Login.Device where

import           Data.Extensible
import           Data.Text           (Text)
import           GitHub.Login.Client (GitHubLoginClient, baseUrl)
import qualified GitHub.Login.Client as GitHub
import           GitHub.Login.Data   (Scope, scopeParam)
import           Lens.Micro          ((^.))
import           Network.HTTP.Req
import           Network.Simple      (buildApi)

type LoginCode = Record
  '[ "device_code"      >: Text
   , "user_code"        >: Text
   , "verification_uri" >: Text
   , "expires_in"       >: Int
   , "interval"         >: Int
   ]

requestCode
  :: (MonadHttp m, GitHubLoginClient c)
  => c
  -> [Scope]
  -> m (GitHub.LoginResponse LoginCode)
requestCode c =
  buildApi c POST (baseUrl c /: "device" /: "code") NoReqBody . scopeParam

type AceesTokenInfo = Record
  '[ "access_token"             >: Text
   , "expires_in"               >: Maybe Int  -- only GitHub App
   , "token_type"               >: Text
   , "scope"                    >: Text
   , "refresh_token"            >: Maybe Text -- only GitHub App
   , "refresh_token_expires_in" >: Maybe Int  -- only GitHub App
   ]

requestAccessToken
  :: (MonadHttp m, GitHubLoginClient c)
  => c
  -> Text -- ^ device code
  -> m (GitHub.LoginResponse AceesTokenInfo)
requestAccessToken c code =
  buildApi c POST (baseUrl c /: "oauth" /: "access_token") NoReqBody params
  where
    params = mconcat
      [ "device_code" =: code
      , "grant_type"  =: ("urn:ietf:params:oauth:grant-type:device_code" :: Text)
      ]

data ErrorCode
  = AuthorizationPendingError
  | SlowDownError
  | ExpiredTokenError
  | UnsupportedGrantTypeError
  | IncorrectClientCredentialsError
  | IncorrectDeviceCodeError
  | AccessDeniedError
  deriving (Show, Eq)

toErrorCode :: GitHub.Error -> Maybe ErrorCode
toErrorCode e = case e ^. #error of
  "authorization_pending"        -> Just AuthorizationPendingError
  "slow_down"                    -> Just SlowDownError
  "expired_token"                -> Just ExpiredTokenError
  "unsupported_grant_type"       -> Just UnsupportedGrantTypeError
  "incorrect_client_credentials" -> Just IncorrectClientCredentialsError
  "incorrect_device_code"        -> Just IncorrectDeviceCodeError
  "access_denied"                -> Just AccessDeniedError
  _                              -> Nothing
