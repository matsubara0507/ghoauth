module Main where

import           GitHub.Login.Test.Client     (TestClient (..))
import           GitHub.Login.Test.MockServer (runMockServer)
import           Spec.GitHub.Login.Device     as GitHub.Login.Device
import           Test.Tasty

main :: IO ()
main = runMockServer $ defaultMain =<< spec

spec :: IO TestTree
spec = testGroup "GitHub.Login" <$> sequence
  [ GitHub.Login.Device.specWith client
  ]
  where
    client = TestClient
