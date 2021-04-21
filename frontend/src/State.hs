{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module State where

import           Common.Auth      (SessionData)
import           Control.Category (Category ((.)))
import           Data.Aeson       (FromJSON, ToJSON)
import           Data.Default     (Default (..))
import           Data.Function    (($))
import           Data.Semigroup   (Semigroup (..))
import           GHC.Generics     (Generic)

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
  } deriving (Generic)

instance FromJSON State
instance ToJSON State

instance Default State where
  def = State
    { stSession = SessionAnon
    }

-- Session

-- TODO: lenses
data Session
  = SessionUser SessionData
  | SessionAnon
  deriving (Generic)

instance FromJSON Session
instance ToJSON Session
