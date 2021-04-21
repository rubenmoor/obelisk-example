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

module DbAdapter where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)
import           Data.Time               (Day, UTCTime)
import Data.ByteString (ByteString)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)
import Text.URI (URI)
import           Data.Password.Argon2    (Argon2, PasswordHash)
import           Data.Password.Instances ()
import           Database.Gerippe        (PersistEntity (EntityField, Unique))
import           GHC.Generics            (Generic)
import qualified Model as Model

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Podcast
  identifier       Text
  UPodcastIdentifier identifier
  blob             ByteString
Platform
  blob             ByteString
  fkPodcast        PodcastId
Episode
  fkPodcast        PodcastId
  slug             Text
  blob             ByteString
  fkEventSource    EventSourceId
User                             -- some real person
  name             Text
  UUserName name
  isSiteAdmin      Bool
  fkEventSource    EventSourceId
  fkDefaultAlias   AliasId Maybe
Alias                            -- one of several identities
  name             Text
  fkUser           UserId
  UAliasName name
Clearance
  fkAlias          AliasId
  fkPodcast        PodcastId
  UClearance fkAlias fkPodcast
  rank             Model.Rank
AuthPwd
  fkUser           UserId
  password         (PasswordHash Argon2)
  UAuthPwdFkUser fkUser
Journal
  blob             ByteString
  fkEventSource    EventSourceId
  fkAlias          AliasId Maybe
EventSource
|]
