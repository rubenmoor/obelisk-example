{-# LANGUAGE DataKinds                  #-}
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

import           Data.Text           (Text)
import           Data.Time           (Day, UTCTime)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)

import           Database.Gerippe    (PersistEntity (EntityField, Unique))
import           Model.Custom        (Event (..), Rank (..), Subject (..),
                                      Visibility (..))
import Data.Password.Argon2 (PasswordHash, Argon2)
import Data.Password.Instances ()

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
