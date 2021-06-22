module Spec.GitHub.Login.Device
    ( specWith
    ) where

import           GitHub.Login.Client      (GitHubLoginClient)
import           GitHub.Login.Device      as GitHub
import           GitHub.Login.Test.Helper (shouldResponseAs)
import           Test.Tasty
import           Test.Tasty.Hspec

specWith :: GitHubLoginClient c => c -> IO TestTree
specWith client = testSpec "GitHub.Login.Device" $ do
  describe "requestCode" $
    it "should return device code" $
      GitHub.requestCode client [] `shouldResponseAs` "test/fixture/code.json"
  describe "requestAccessToken" $
    it "should return access token" $
      GitHub.requestAccessToken client "hoge" `shouldResponseAs` "test/fixture/token.json"
