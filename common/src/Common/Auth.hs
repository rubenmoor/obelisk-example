{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Auth
  ( CompactJWT (..)
  , Credentials (..)
  ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype CompactJWT = CompactJWT
  { unCompactJWT :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

data Credentials = Credentials
  { credName     :: Text
  , credPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Credentials
instance ToJSON Credentials
