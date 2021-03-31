{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model
  ( module Model
  , module Model.Custom
  , Unique (..)
  , EntityField (..)
  ) where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import           Data.Time               (Day, UTCTime)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)

import           Data.Password.Argon2    (Argon2, PasswordHash)
import           Data.Password.Instances ()
import           Database.Gerippe        (PersistEntity (EntityField, Unique))
import           GHC.Generics            (Generic)
import           Model.Custom            (Event (..), Rank (..), Subject (..),
                                          Visibility (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Podcast
  identifier       Text
  UPodcastIdentifier identifier
  title            Text
  description      Text
  copyright        Text
  email            Text
  licence          Text
  pubDate          UTCTime
  itunesSubtitle   Text
  itunesSummary    Text
  authors          Text
  itunesOwnerNames Text
  keywords         Text
Episode
  fkPodcast        PodcastId
  title            Text
  slug             Text
  UEpisodeSlug slug
  customIndex      Text
  UCustomIndex customIndex
  ftExtension      Text
  audioContentType Text
  thumbnailFile    FilePath
  descriptionShort Text
  descriptionLong  Text
  duration         Int           -- duration in seconds
  fileSize         Int           -- file size in bytes
  pubdate          Day           -- day of recording
  created          UTCTime
  videoUrl         Text
  visibility       Visibility
  fkEventSource    EventSourceId
User                             -- some real person
  name             Text
  UUserName name
  isSiteAmdin      Bool
  fkEventSource    EventSourceId
Alias                            -- one of several identities
  name             Text
  fkUser           UserId
  UAliasName name
  deriving Generic
Clearance
  fkAlias          AliasId
  fkPodcast        PodcastId
  UClearance fkAlias fkPodcast
  rank             Rank
AuthPwd
  fkUser           UserId
  password         (PasswordHash Argon2)
  UAuthPwdFkUser fkUser
Journal
  fkEventSource    EventSourceId
  fkAlias          AliasId Maybe
  created          UTCTime
  subject          Subject
  event            Event
  description      Text
EventSource
|]

instance FromJSON Alias
instance ToJSON Alias
