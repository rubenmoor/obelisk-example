{-# LANGUAGE DeriveGeneric         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model
  ( module Model
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
  { journalCreated     :: UTCTime
  , journalSubject     :: Subject
  , journalEvent       :: Event
  , journalDescription :: Text
  } deriving (Generic)

instance ToJSON Journal
instance FromJSON Journal

data Event
  = EventView
  | EventLogin
  | EventLogout
  | EventCreation
  | EventEdit
  deriving (Generic)

instance ToJSON Event
instance FromJSON Event

-- subject of journal entry
data Subject
  = SubjectUser -- cases: new user, user changes password, receives new clearance
  | SubjectAlias
  | SubjectEpisode
  | SubjectPodcast
  deriving (Generic)

instance ToJSON Subject
instance FromJSON Subject

instance FromJSON URI where
  parseJSON (String str) = case mkURI str of
    Left e  -> fail $ show e
    Right u -> pure u
  parseJSON o = unexpected o

instance ToJSON URI where
  toJSON = toJSON . render
