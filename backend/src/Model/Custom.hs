{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Custom
  ( Visibility (..)
  , Rank (..)
  , Event (..)
  ) where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Show (Show)
import Text.Read (Read)
import Database.Persist.TH (derivePersistField)

data Visibility
  = VisibilityPublic
  | VisibilityHidden
  deriving (Eq, Ord, Show, Read)

derivePersistField "Visibility"

data Rank
  = RankModerator
  | RankAdmin
  | RankOwner
  deriving (Eq, Ord, Show, Read)

derivePersistField "Rank"

data Event
  = EventView
  | EventLogin
  | EventLogout
  | EventCreation
  | EventEdit
  deriving (Eq, Ord, Show, Read)

derivePersistField "Event"
