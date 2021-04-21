{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model.Custom
  ( Visibility (..)
  , Rank (..)
  , Event (..)
  , Subject (..)
  , PlatformName (..)
  ) where

import           Control.Category        (Category ((.)))
import           Data.Aeson              (Value(String), FromJSON (..), ToJSON (..))
import           Data.Either             (Either(..))
import           Data.Either.Combinators (mapLeft)
import           Data.Eq                 (Eq)
import           Data.Function           (($))
import           Data.Ord                (Ord)
import           Data.Semigroup          (Semigroup ((<>)))
import qualified Data.Text               as Text
import           Database.Gerippe        (PersistField (..),
                                          PersistFieldSql (..),
                                          PersistValue (PersistText),
                                          SqlType (..))
import           Database.Persist.TH     (derivePersistField)
import           GHC.Generics            (Generic)
import           Text.Read               (Read)
import           Text.Show               (Show (show))
import           Text.URI                (URI, mkURI, render)
import Control.Applicative (Applicative(pure))
import Control.Monad.Fail (MonadFail(fail))
import Data.Aeson.Types (unexpected)
import Data.Aeson.Encoding (string)

data Visibility
  = VisibilityPublic
  | VisibilityHidden
  deriving (Eq, Ord, Show, Read, Generic)

derivePersistField "Visibility"

instance FromJSON Visibility
instance ToJSON Visibility

data Rank
  = RankModerator
  | RankAdmin
  | RankOwner
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Rank
instance ToJSON Rank

derivePersistField "Rank"

data Event
  = EventView
  | EventLogin
  | EventLogout
  | EventCreation
  | EventEdit
  deriving (Eq, Ord, Show, Read)

derivePersistField "Event"

-- subject of journal entry
data Subject
  = SubjectUser -- cases: new user, user changes password, receives new clearance
  | SubjectAlias
  | SubjectEpisode
  | SubjectPodcast
  deriving (Eq, Ord, Show, Read)

derivePersistField "Subject"

data PlatformName
  = PlatformTelegram
  | PlatformSpotify
  | PlatformItunes
  | PlatformYoutube
  deriving (Show, Read, Generic)

instance FromJSON PlatformName
instance ToJSON PlatformName

derivePersistField "PlatformName"

instance PersistField URI where
  toPersistValue = PersistText . render
  fromPersistValue (PersistText str) = mapLeft (Text.pack . show) $ mkURI str
  fromPersistValue other = Left $ "Expected PersistText, received: " <> Text.pack (show other)

instance PersistFieldSql URI where
  sqlType _ = SqlString

instance FromJSON URI where
  parseJSON (String str) = case mkURI str of
    Left e -> fail $ show e
    Right u -> pure u
  parseJSON o = unexpected o

instance ToJSON URI where
  toJSON = toJSON . render
