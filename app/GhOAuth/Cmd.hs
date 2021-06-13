{-# LANGUAGE NoImplicitPrelude #-}

module GhOAuth.Cmd where

import           RIO

import           GhOAuth.Env
import qualified GitHub.Login        as GitHub
import qualified GitHub.Login.Device as GitHub
import           Mix.Plugin.Logger   as MixLogger
import           Network.HTTP.Req
import           RIO.Time

cmd :: RIO Env ()
cmd = do
  client <- asks (view #client)
  resp <- runReq defaultHttpConfig $ GitHub.requestCode client []
  case responseBody resp of
    GitHub.Err e ->
      MixLogger.logError $ display (e ^. #error_description <> " please see: " <> e ^. #error_uri)
    GitHub.Ok code -> do
      MixLogger.logInfo $ display ("Copy code: " <> code ^. #user_code)
      MixLogger.logInfo $ display ("then open: " <> code ^. #verification_uri)
      now <- getCurrentTime
      result <- pollAceesTokenInfo (addUTCTime (fromIntegral $ code ^. #expires_in) now) code
      case result of
        GitHub.Err e ->
          MixLogger.logError $ display (e ^. #error_description <> " please see: " <> e ^. #error_uri)
        GitHub.Ok info ->
          MixLogger.logInfo $ display ("Access token: " <> info ^. #access_token)

pollAceesTokenInfo :: UTCTime -> GitHub.LoginCode -> RIO Env (GitHub.Ok GitHub.AceesTokenInfo)
pollAceesTokenInfo expireAt code = go
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

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command.\n"
