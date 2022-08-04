{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module State where

import           Common.Auth      (SessionData)
import           Control.Category (Category ((.)))
import           Data.Aeson       (KeyValue((.=)), object, (.:), Value(..), FromJSON (..), ToJSON (..))
import           Data.Default     (Default (..))
import           Data.Function    (($))
import           Data.Semigroup   (Semigroup (..))
import           GHC.Generics     (Generic)
import Data.Functor (Functor (fmap))
import Data.Text (Text)
import Data.Maybe (Maybe(Nothing))
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Data.Aeson.Types (typeMismatch, prependFailure)
import Reflex.Dom (Reflex, EventWriter, Event, tellEvent)

-- frontend application state

newtype EStateUpdate
  = EStateUpdate { unEStateUpdate :: State -> State }

instance Semigroup EStateUpdate where
  u <> v =
    let u' = unEStateUpdate u
        v' = unEStateUpdate v
    in  EStateUpdate $ v' . u'

-- State

data State = State
  { stSession :: Session
  , stMsg :: Maybe Text
  } deriving (Generic)

instance FromJSON State where
  parseJSON (Object o) =
    State <$> o .: "session"
          <*> pure Nothing -- don't expect a persisted message
  parseJSON invalid =
    prependFailure "parsing State failed, " $ typeMismatch "Object" invalid

instance ToJSON State where
  toJSON State { stSession = session, stMsg = _ } =
    object
    [ "session" .= session
    -- stMsg: never persist messages
    ]

instance Default State where
  def = State
    { stSession = SessionAnon
    , stMsg = Nothing
    }

-- Session

-- TODO: lenses
data Session
  = SessionUser SessionData
  | SessionAnon
  deriving (Generic)

instance FromJSON Session
instance ToJSON Session

updateState
  :: ( Reflex t
     , EventWriter t EStateUpdate m
     )
  => Event t (State -> State)
  -> m ()
updateState event =
  tellEvent $ fmap EStateUpdate event
