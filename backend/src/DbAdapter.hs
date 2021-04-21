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

import           Data.ByteString         (ByteString)
import           Data.Function           (on)
import           Data.Password.Argon2    (Argon2, PasswordHash)
import           Data.Password.Instances ()
import           Data.Text               (Text)
import           Database.Persist.TH     (mkMigrate, mkPersist,
                                          persistLowerCase, share, sqlSettings)
import DbAdapter.Instances ()
import qualified Model

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Podcast
  identifier       Text
  UPodcastIdentifier identifier
  blob             ByteString
Platform
  blob             ByteString
  fkPodcast        PodcastId
Episode
  slug             Text
  blob             ByteString
  fkPodcast        PodcastId
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

instance Eq Podcast where
  (==) = on (==) podcastIdentifier

instance Ord Podcast where
  compare = on compare podcastIdentifier
