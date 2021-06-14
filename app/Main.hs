{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Paths_ghoauth          (version)
import           RIO

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import           GhOAuth.Cmd
import qualified GitHub.Login           as GitHub
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           System.Environment     (lookupEnv)
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version <> "\n")
     | otherwise     -> runCmd r (listToMaybe args)
  where
    opts = #help      @= helpOpt
        <: #version   @= versionOpt
        <: #verbose   @= verboseOpt
        <: #client_id @= clientIdOpt
        <: #env_file  @= envFileOpt
        <: #env_var   @= envVarOpt
        <: nil

type Options = Record
  '[ "help"      >: Bool
   , "version"   >: Bool
   , "verbose"   >: Bool
   , "client_id" >: Maybe Text
   , "env_file"  >: FilePath
   , "env_var"   >: String
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

clientIdOpt :: OptDescr' (Maybe Text)
clientIdOpt = fmap fromString <$> optLastArg [] ["client_id"] "TEXT" "GitHub Apps client ID instead of CLIENT_ID environment variable"

envFileOpt :: OptDescr' FilePath
envFileOpt = fromMaybe "~/.env" <$> optLastArg [] ["env-file"] "PATH" ".env file path to write access token"

envVarOpt :: OptDescr' String
envVarOpt = fromMaybe "GITHUB_TOKEN" <$> optLastArg [] ["env-var"] "TEXT" "Environment variable name for access token"

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts _path = do
  clientIdEnv <- fmap fromString <$> lookupEnv "CLIENT_ENV"
  case opts ^. #client_id <|> clientIdEnv of
    Nothing    -> fail "not found CLIENT_ID"
    (Just cid) -> do
      let plugin = hsequence
                 $ #logger <@=> MixLogger.buildPlugin logOpts
                <: #client <@=> pure (GitHub.newClient cid)
                <: #dotenv <@=> pure (#path @= (opts ^. #env_file) <: #var @= (opts ^. #env_var) <: nil)
                <: nil
      Mix.run plugin cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
