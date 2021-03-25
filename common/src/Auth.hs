{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Auth
  ( CompactJWT (..)
  , Credentials (..)
  , RespLogin (..)
  , LoginFailure (..)
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

-- login response type

data RespLogin
  = RespLoginSuccess CompactJWT
  | RespLoginFailure LoginFailure
  deriving (Eq, Show, Generic)

instance FromJSON RespLogin
instance ToJSON RespLogin

data LoginFailure
  = LoginFailureDoesNotExist
  | LoginFailureWrongPassword
  deriving (Eq, Show, Generic)

instance FromJSON LoginFailure
instance ToJSON LoginFailure
