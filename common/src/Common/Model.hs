{-# LANGUAGE DeriveGeneric         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.Model
  ( module Common.Model
  ) where

import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (String))
import           Data.Aeson.Types        (unexpected)
import           Data.Text               (Text)
import           Data.Time               (Day, UTCTime)
import           GHC.Generics            (Generic)
import           Text.URI                (URI, mkURI, render)

data Podcast = Podcast
  { podcastIdentifier       :: Text
  , podcastTitle            :: Text
  , podcastDescription      :: Text
  , podcastCopyright        :: Text
  , podcastEmail            :: Text
  , podcastLicence          :: Text
  , podcastPubDate          :: UTCTime
  , podcastItunesSubtitle   :: Text
  , podcastItunesSummary    :: Text
  , podcastAuthors          :: Text
  , podcastItunesOwnerNames :: Text
  , podcastKeywords         :: Text
  } deriving (Generic)

instance ToJSON Podcast
instance FromJSON Podcast

-- Platform

data Platform = Platform
  { platformName :: PlatformName
  , platformLink :: URI
  } deriving (Generic)

instance ToJSON Platform
instance FromJSON Platform

data PlatformName
  = PlatformTelegram
  | PlatformSpotify
  | PlatformItunes
  | PlatformYoutube
  deriving (Generic)

instance FromJSON PlatformName
instance ToJSON PlatformName

-- Episode

data Episode = Episode
  { episodeTitle            :: Text
  , episodeSlug             :: Text
  , episodeCustomIndex      :: Text
  , episodeFtExtension      :: Text
  , episodeAudioContentType :: Text
  , episodeThumbnailFile    :: FilePath
  , episodeDescriptionShort :: Text
  , episodeDescriptionLong  :: Text
  , episodeDuration         :: Int           -- duration in seconds
  , episodeFileSize         :: Int           -- file size in bytes
  , episodePubdate          :: Day           -- day of recording
  , episodeCreated          :: UTCTime
  , episodeVideoUrl         :: Text
  , episodeVisibility       :: Visibility
  } deriving (Generic)

instance ToJSON Episode
instance FromJSON Episode

data Visibility
  = VisibilityPublic
  | VisibilityHidden
  deriving (Generic)

instance FromJSON Visibility
instance ToJSON Visibility

data Rank
  = RankModerator
  | RankAdmin
  | RankOwner
  deriving (Generic, Eq, Ord)

instance FromJSON Rank
instance ToJSON Rank

-- Journal

data Journal = Journal
  { journalEvent :: JournalEvent
  , journalVisitorId :: Int
  , journalVisitorIp :: Text
  , journalMAliasUser :: Maybe (Text, Text)
  , journalTime :: UTCTime
  } deriving (Generic)

instance FromJSON Journal
instance ToJSON Journal

data JournalEvent
  = EventUser EventUser
  | EventApp  EventApp
  deriving (Generic)

instance ToJSON JournalEvent
instance FromJSON JournalEvent

data EventUser
  = EventLogin
  | EventLogout
  | EventSignup
  | EventEdit Text Text Text
  | EventDelete
  deriving (Generic)

instance ToJSON EventUser
instance FromJSON EventUser

data EventApp
  = EventViewPage Text
  | EventEpisodeNew Text
  | EventPodcastNew Text
  deriving (Generic)

instance ToJSON EventApp
instance FromJSON EventApp

instance FromJSON URI where
  parseJSON (String str) = case mkURI str of
    Left e  -> fail $ show e
    Right u -> pure u
  parseJSON o = unexpected o

instance ToJSON URI where
  toJSON = toJSON . render
