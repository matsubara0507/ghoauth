{-# LANGUAGE NoImplicitPrelude #-}

module GhOAuth.Clipboard
  ( setClipboard
  ) where

import           RIO
import           RIO.Process

import           System.Info (os)

setClipboard ::
  (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, HasCallStack)
  => Text -> m Bool
setClipboard txt =
  case os of
    "darwin"  -> setClipboardBy "pbcopy" txt
    "linux"   -> setClipboardBy "xclip" txt
    "mingw32" -> setClipboardBy "clip" txt
    _         -> pure False

setClipboardBy ::
  (HasProcessContext env, HasLogFunc env, MonadReader env m, MonadUnliftIO m, HasCallStack)
  => FilePath -> Text -> m Bool
setClipboardBy cmd txt = do
  r <- doesExecutableExist cmd
  if r then do
    let input = byteStringInput $ fromStrictBytes (encodeUtf8 txt)
    code <- proc cmd [] (runProcess . setStdin input)
    pure $ code == ExitSuccess
  else
    pure False
