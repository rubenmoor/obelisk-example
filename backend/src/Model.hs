{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
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
{-# LANGUAGE UndecidableInstances       #-}

module Model where

import           Data.ByteString     (ByteString)
import           Data.Default        (Default (..))
import           Data.Text           (Text)
import           Data.Time           (Day (ModifiedJulianDay), UTCTime (..),
                                      secondsToDiffTime)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)

import           Model.Custom        (Visibility (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Episode
  title            Text
  slug             Text
  UniqueSlug slug
  customIndex      Text
  UniqueCustomIndex customIndex
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
User
  name             Text
  UniqueName name
AuthPwd
  fkUser           UserId
  password         ByteString
  UniqueFkUser fkUser
|]

instance Default Episode where
  def =
    let episodeTitle = ""
        episodeSlug = ""
        episodeCustomIndex = ""
        episodeFtExtension = ""
        episodeAudioContentType = ""
        episodeThumbnailFile = ""
        episodeDescriptionShort = ""
        episodeDescriptionLong = ""
        episodeDuration = 0
        episodeFileSize = 0
        episodePubdate = ModifiedJulianDay 0
        episodeCreated = UTCTime
          { utctDay = ModifiedJulianDay 0
          , utctDayTime = secondsToDiffTime 0
          }
        episodeVideoUrl = ""
        episodeVisibility = VisibilityHidden
    in  Episode{..}
