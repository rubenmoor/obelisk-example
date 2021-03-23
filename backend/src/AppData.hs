{-# LANGUAGE NoImplicitPrelude #-}

module AppData
  ( module AppData
  ) where

import           Control.Monad.Reader   (ReaderT)
import           Data.Pool              (Pool)
import           Data.Text              (Text)
import           Database.Persist.MySQL (SqlBackend)
import           Snap.Core              (Snap)
import           System.IO              (FilePath)

data EnvApplication = EnvApplication
  { envPool      :: Pool SqlBackend
  , envMediaDir  :: FilePath
  , envUrl       :: Text
  , envPublicDir :: FilePath
  }

type Handler a = ReaderT EnvApplication Snap a
