{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model
  ( module Model
  , module Model.Custom
  ) where

import           Data.ByteString     (ByteString)
import           Data.Text           (Text)
import           Data.Time           (Day, UTCTime)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)

import           Model.Custom        (Visibility (..), Rank (..), Event (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
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
Alias                            -- one of several identities
  name             Text
  rank             Rank
  fkUser           UserId
  UAliasName name
AuthPwd
  fkUser           UserId
  password         ByteString
  UAuthPwdFkUser fkUser
Journal
  fkEventSource    EventSourceId
  fkUser           UserId Maybe
  created          UTCTime
  event            Event
  description      Text
EventSource
|]
