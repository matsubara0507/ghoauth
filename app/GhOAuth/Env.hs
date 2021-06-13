{-# LANGUAGE NoImplicitPrelude #-}

module GhOAuth.Env where

import           RIO

import           Data.Extensible
import           GitHub.Login    as GitHub

type Env = Record
  '[ "logger" >: LogFunc
   , "client" >: GitHub.LoginClient
   ]
