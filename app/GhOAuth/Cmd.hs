{-# LANGUAGE NoImplicitPrelude #-}

module GhOAuth.Cmd where

import           RIO
import qualified RIO.ByteString       as BS
import qualified RIO.List             as List
import qualified RIO.Text             as Text

import qualified Configuration.Dotenv as Dotenv
import           GhOAuth.Clipboard    (setClipboard)
import           GhOAuth.Env
import qualified GitHub.Login         as GitHub
import qualified GitHub.Login.Device  as GitHub
import           Mix.Plugin.Logger    as MixLogger
import           Network.HTTP.Req
import           RIO.Time
import           Web.Browser          (openBrowser)

cmd :: RIO Env ()
cmd = do
  r <- fetchAccessTokenInfo
  case r of
    GitHub.Err e ->
      MixLogger.logError $ display (e ^. #error_description <> " please see: " <> e ^. #error_uri)
    GitHub.Ok info -> do
      updateDotEnv info
      MixLogger.logInfo "Success"

fetchAccessTokenInfo :: RIO Env (GitHub.Ok GitHub.AccessTokenInfo)
fetchAccessTokenInfo = do
  client <- asks (view #client)
  resp <- runReq defaultHttpConfig $ GitHub.requestCode client []
  case responseBody resp of
    GitHub.Err e ->
      pure (GitHub.Err e)
    GitHub.Ok code -> do
      BS.putStr $ encodeUtf8 ("Copy code: " <> code ^. #user_code <> "\n")
      BS.putStr $ encodeUtf8 ("then open: " <> code ^. #verification_uri <> "\n")
      whenM (asks $ view #useClipboard) $
        unlessM (setClipboard (code ^. #user_code)) $
          MixLogger.logWarn "Failure: set user_code to clipboard"
      unlessM (openBrowser' code) $
          MixLogger.logWarn "Failure: open browser"
      now <- getCurrentTime
      pollAccessTokenInfo (addUTCTime (fromIntegral $ code ^. #expires_in) now) code

openBrowser' :: MonadIO m => GitHub.LoginCode -> m Bool
openBrowser' code = liftIO $ openBrowser (Text.unpack $ code ^. #verification_uri)

pollAccessTokenInfo :: UTCTime -> GitHub.LoginCode -> RIO Env (GitHub.Ok GitHub.AccessTokenInfo)
pollAccessTokenInfo expireAt code = go
  where
    requestWithSleep client = do
      threadDelay (code ^. #interval * 1_000_000)
      resp <- runReq defaultHttpConfig $ GitHub.requestAccessToken client (code ^. #device_code)
      pure $ responseBody resp
    shouldContinue e now =
      GitHub.toErrorCode e == Just GitHub.AuthorizationPendingError || now < expireAt
    go = do
      client <- asks (view #client)
      resp   <- requestWithSleep client
      now    <- getCurrentTime
      case resp of
        GitHub.Err e | shouldContinue e now -> go
        _                                   -> pure resp

updateDotEnv :: GitHub.AccessTokenInfo -> RIO Env ()
updateDotEnv info = do
  conf <- asks (view #dotenv)
  envs <- Dotenv.parseFile (conf ^. #path)
  let token = Text.unpack $ info ^. #access_token
  case List.find (\(k, _) -> k == conf ^. #var) envs of
    Nothing ->
      writeDotEnv $ envs ++ [(conf ^. #var, token)]
    Just (_, val) -> do
      BS.putStr $
        fromString ("Replace? (" <> conf ^. #path <> "): " <> hiddenFrom 4 val <> "\n")
      BS.putStr "(yes/N)> "
      confirm <- BS.getLine
      if confirm == "yes" then
        writeDotEnv $ map (\(k, v) -> (k, if k == conf ^. #var then token else v)) envs
      else
        MixLogger.logError "Canceled"

hiddenFrom :: Int -> String -> String
hiddenFrom n t = hd ++ map (const '*') tl
  where
    (hd, tl) = List.splitAt n t

writeDotEnv :: [(String, String)] -> RIO Env ()
writeDotEnv envs = do
  conf <- asks (view #dotenv)
  let content = List.unlines $ map (\(k, v) -> k ++ "=" ++ v) envs
  writeFileUtf8 (conf ^. #path) $ fromString content

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
