{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Common.Api where

import           Common.Auth              (AuthRequired, LoginData, SessionData,
                                           UserNew)
import           Control.Applicative      (Applicative (pure))
import           Control.Category         (Category (id))
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Bool                (Bool)
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Maybe               (Maybe)
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           Common.Model             (Episode, Platform, Podcast)
import           Network.HTTP.Media       ((//), (/:))
import           Servant.API              ((:<|>) (..), (:>), Get, JSON, Post,
                                           Raw, ReqBody)
import           Servant.API.Capture      (Capture)
import           Servant.API.ContentTypes (Accept (..), MimeRender (..),
                                           MimeUnrender (..))

type RouteShow = "show" :> Capture "podcast_id" Text :>
  ( "feed.xml" :> Get '[XML] Lazy.ByteString
  )

type RouteMedia = "media" :> Raw

type RoutesApi = "api" :>
  (
         RoutesAuth
    :<|> RoutesSerendipity
    :<|> RoutesUser
  )

type RoutesAuth = "auth" :>
    (      "login"  :> ReqBody '[JSON] LoginData :> Post '[JSON] (Maybe SessionData)
      :<|> "new"    :> ReqBody '[JSON] UserNew   :> Post '[JSON] SessionData
      :<|> "exists" :> ReqBody '[JSON] Text      :> Post '[JSON] Bool
      :<|> "alias" :> "exists" :> ReqBody '[JSON] Text      :> Post '[JSON] Bool

      -- logout doesn't do anything server-side apart from journaling
      :<|> AuthRequired "jwt" :> "logout" :> Post '[JSON] ()

    )

type RoutesUser = "alias" :>
    (      AuthRequired "jwt" :> "rename"       :> ReqBody '[JSON] Text :> Post '[JSON] Text
      :<|> AuthRequired "jwt" :> "get" :> "all" :> Get '[JSON] [Text]
      :<|> AuthRequired "jwt" :> "setDefault"   :> ReqBody '[JSON] Text :> Post '[JSON] ()
      :<|> AuthRequired "jwt" :> "visibility"   :> ReqBody '[JSON] Bool :> Post '[JSON] ()
    )

type RoutesSerendipity =
        AuthRequired "jwt" :> "episode" :> Capture "podcast_id" Text :> "new" :> ReqBody '[JSON] EpisodeNew :> Post '[JSON] ()
   :<|> AuthRequired "jwt" :> "podcast" :> "new" :> ReqBody '[JSON] Text :> Post '[JSON] ()
   :<|> "podcast" :> Capture "podcast_id" Text :> Get '[JSON] (Podcast, [Platform], [Episode])

-- episode

data EpisodeNew = EpisodeNew
  { newCustomIndex :: Text
  , newTitle       :: Text
  , newDate        :: Text
  } deriving (Generic)

instance FromJSON EpisodeNew
instance ToJSON EpisodeNew

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance MimeRender XML Lazy.ByteString where
  mimeRender _ = id

instance MimeUnrender XML Lazy.ByteString where
  mimeUnrender _ bs = pure bs
