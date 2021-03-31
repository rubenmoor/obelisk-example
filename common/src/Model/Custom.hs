{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model.Custom
  ( Visibility (..)
  , Rank (..)
  , Event (..)
  , Subject (..)
  ) where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Eq             (Eq)
import           Data.Ord            (Ord)
import           Database.Persist.TH (derivePersistField)
import           GHC.Generics        (Generic)
import           Text.Read           (Read)
import           Text.Show           (Show)

data Visibility
  = VisibilityPublic
  | VisibilityHidden
  deriving (Eq, Ord, Show, Read)

derivePersistField "Visibility"

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
