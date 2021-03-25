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
  }

type Handler = ReaderT EnvApplication Snap
