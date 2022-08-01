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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies               #-}

module DbAdapter where

import           Data.ByteString         (ByteString)
import           Data.Function           (on)
import           Data.Password.Argon2    (Argon2, PasswordHash)
import           Data.Password.Instances ()
import           Data.Text               (Text)
import           Database.Persist.TH     (mkEntityDefList, mkPersist,
                                          persistLowerCase, share, sqlSettings)
import Data.Time (UTCTime)

import DbAdapter.Instances ()
import qualified Common.Model as Model

share [mkPersist sqlSettings, mkEntityDefList "entities"] [persistLowerCase|
Podcast
  identifier       Text
  UPodcastIdentifier identifier
  blob             ByteString
Platform
  blob             ByteString
  fkPodcast        PodcastId
  -- optional explicit foreign key constraint
  Foreign Podcast fkcPodcast fkPodcast
Episode
  slug             Text
  blob             ByteString
  fkPodcast        PodcastId
User                             -- some real person
  name             Text
  UUserName name
  isSiteAdmin      Bool
  fkDefaultAlias   AliasId Maybe
Alias                            -- one of several identities
  name             Text
  fkUser           UserId
  lastEdited       UTCTime
  isVisible        Bool
  UAliasName name
Visitor
  ipAddress        Text
  UIpAddress ipAddress
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
  created          UTCTime
  blob             ByteString
  fkVisitor        VisitorId
  fkMAlias         AliasId Maybe
|]

instance Eq Podcast where
  (==) = on (==) podcastIdentifier

instance Ord Podcast where
  compare = on compare podcastIdentifier
