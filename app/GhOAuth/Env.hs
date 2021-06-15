{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhOAuth.Env where

import           RIO
import           RIO.Process

import           Data.Extensible
import           GitHub.Login    as GitHub

type Env = Record
  '[ "logger"         >: LogFunc
   , "client"         >: GitHub.LoginClient
   , "dotenv"         >: DotEnvConfig
   , "processContext" >: ProcessContext
   , "useClipboard"   >: Bool
   ]

type DotEnvConfig = Record
  '[ "path" >: FilePath
   , "var"  >: String
   ]

instance Lookup xs "processContext" ProcessContext => HasProcessContext (Record xs) where
  processContextL = lens (view #processContext) (\x y -> x & #processContext `set` y)
