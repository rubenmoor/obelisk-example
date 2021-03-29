{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Common.Auth
  ( CompactJWT (..)
  , Credentials (..)
  ) where

import           Control.Arrow      (ArrowChoice ((+++)))
import           Data.Aeson         (FromJSON, ToJSON)
import qualified Data.ByteString    as B
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import           GHC.Generics       (Generic)
import           Web.HttpApiData    (FromHttpApiData (..), ToHttpApiData (..))

newtype CompactJWT = CompactJWT
  { unCompactJWT :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

instance (FromHttpApiData CompactJWT) where
    parseQueryParam = Right . CompactJWT
    parseHeader =
      let toCompactJWT str =
            CompactJWT $ fromMaybe str $ Text.stripPrefix "Bearer " str
      in  (Text.pack . show +++ toCompactJWT) . Text.decodeUtf8'
instance (ToHttpApiData CompactJWT) where
    toQueryParam (CompactJWT t) = t
    toHeader (CompactJWT t) = "Bearer " <> Text.encodeUtf8 t

data Credentials = Credentials
  { credName     :: Text
  , credPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Credentials
instance ToJSON Credentials
