{-# LANGUAGE NoImplicitPrelude #-}

module AppData
  ( module AppData
  ) where

import           Control.Monad.Reader   (ReaderT)
import           Data.Pool              (Pool)
import           Data.Text              (Text)
import           Database.Persist.MySQL (SqlBackend)
import           Snap.Core              (Snap)
import           System.IO              (IO, FilePath)
import Crypto.JWT (JWK)

data EnvApplication = EnvApplication
  { envPool      :: Pool SqlBackend
  , envMediaDir  :: FilePath
  , envUrl       :: Text
  , envJwk       :: JWK
  }

type Handler = ReaderT EnvApplication Snap
type DbAction = ReaderT SqlBackend IO
