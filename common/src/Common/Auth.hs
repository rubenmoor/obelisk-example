{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Common.Auth
  ( CompactJWT (..)
  , LoginData (..)
  , UserNew (..)
  , AuthProtect
  , UserInfo (..)
  ) where

import           Control.Arrow           (ArrowChoice ((+++)))
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Map                (Map)
import           Data.Maybe              (fromMaybe)
import           Data.Proxy              (Proxy (Proxy))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           Reflex.Dom              (Reflex (Dynamic))
import           Servant                 (HasContextEntry (getContextEntry))
import           Servant.API             ((:>))
import           Servant.Common.Req      (addHeader)
import           Servant.Reflex          (HasClient (..))
import           Servant.Server          (HasServer (..))
import           Servant.Server.Internal (DelayedM, addAuthCheck, withRequest)
import           Snap.Core               (Request, Snap)
import           Snap.Internal.Core      (evalSnap)
import           Web.HttpApiData         (FromHttpApiData (..),
                                          ToHttpApiData (..))
import Model (Alias, Rank)
import Database.Gerippe (PersistEntity(Key))

newtype CompactJWT = CompactJWT
  { unCompactJWT :: Text
  } deriving (Eq, Show, FromJSON, ToJSON)

instance (FromHttpApiData CompactJWT) where
    parseQueryParam = Right . CompactJWT
    parseHeader =
      let toCompactJWT str =
            CompactJWT $ fromMaybe str $ Text.stripPrefix "Bearer " str
      in  (Text.pack . show +++ toCompactJWT) . Text.decodeUtf8'
instance (ToHttpApiData CompactJWT) where
    toQueryParam (CompactJWT t) = t
    toHeader (CompactJWT t) = "Bearer " <> Text.encodeUtf8 t

data LoginData = LoginData
  { ldUserName :: Text
  , ldPassword :: Text
  , ldAliasName :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginData
instance ToJSON LoginData

data UserNew = UserNew
  { unUserName     :: Text
  , unPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserNew
instance ToJSON UserNew

data AuthProtect (tag :: k) deriving (Typeable)

instance (HasClient t m api tag, Reflex t)
      => HasClient t m (AuthProtect realm :> api) tag where

  type Client t m (AuthProtect realm :> api) tag =
    Dynamic t (Either Text CompactJWT) -> Client t m api tag

  clientWithRouteAndResultHandler Proxy q t req baseurl opts wrap token =
    clientWithRouteAndResultHandler (Proxy :: Proxy api) q t req' baseurl opts wrap
      where
        req' = addHeader "Authorization" token req

data UserInfo = UserInfo
  { uiIsSiteAdmin :: Bool
  , uiUserName    :: Text
  , uiAlias       :: Alias
  , uiKeyAlias    :: Key Alias
  , uiClearances  :: Map Text Rank
  }

-- type instance AuthServerData (AuthProtect "jwt") = UserInfo

instance ( HasServer api context m
         , HasContextEntry context (Snap UserInfo)
         )
  => HasServer (AuthProtect tag :> api) context m where

  type ServerT (AuthProtect tag :> api) context m =
    UserInfo -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route (Proxy :: Proxy (AuthProtect tag :> api)) context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` withRequest authCheck)
      where
        authCheck :: Request -> DelayedM m UserInfo
        authCheck =
          liftIO . evalSnap (getContextEntry context)
                            (\x -> pure $! (x `seq` ()))
                            (\f -> let !_ = f 0 in pure ())
