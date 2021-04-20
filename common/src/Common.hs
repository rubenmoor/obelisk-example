{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Common where

import           Common.Auth              (AuthProtect, CompactJWT, LoginData, UserNew, UserInfo)
import           Control.Applicative      (Applicative (pure))
import           Control.Category         (Category (id))
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Bool                (Bool)
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Data                (Proxy (Proxy))
import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            (($))
import           Data.Int                 (Int)
import           Data.Maybe               (Maybe)
import           Data.Text                (Text, replace)
import qualified Data.Text                as Text
import           GHC.Generics             (Generic)
import           GHC.Num                  (Num ((*)))
import           GHC.Real                 (Integral (div, mod))
import           Network.HTTP.Media       ((//), (/:))
import           Servant.API              ((:<|>) (..), (:>), Get, JSON, Post,
                                           ReqBody)
import           Servant.API.Capture      (Capture)
import           Servant.API.ContentTypes (Accept (..), MimeRender (..),
                                           MimeUnrender (..))
import           Text.Printf              (printf)
import           Text.Read                (Read)
import           Text.Show                (Show)
import Model (Podcast)

type Routes =
       RouteShow
  :<|> RoutesApi


type RouteShow = "show" :> Capture "podcast_id" Text :>
  ( "feed.xml" :> Get '[XML] Lazy.ByteString
  )

type RoutesApi = "api" :>
  (     "auth" :>
    (      RouteGrantAuthPwd
      :<|> RouteUserNew
      :<|> RouteDoesUserExist
    )
   :<|> "epsiode" :> Capture "podcast_id" Text :>
    (      AuthProtect "jwt" :> RouteEpisodeNew
    )
   :<|> "podcast" :>
    (
           AuthProtect "jwt" :> RoutePodcastNew
      :<|> RoutePodcastGet
    )
   :<|> "alias" :>
    (      AuthProtect "jwt" :> RouteAliasRename
      :<|> AuthProtect "jwt" :> RouteAliasGetAll
      :<|> AuthProtect "jwt" :> RouteAliasSetDefault
    )
  )

type RouteGrantAuthPwd  = "login"  :> ReqBody '[JSON] LoginData :> Post '[JSON] (Maybe (CompactJWT, UserInfo))
type RouteUserNew       = "new"    :> ReqBody '[JSON] UserNew   :> Post '[JSON] (CompactJWT, UserInfo)
type RouteDoesUserExist = "exists" :> ReqBody '[JSON] Text      :> Post '[JSON] Bool

type RouteAliasRename     = "rename"       :> ReqBody '[JSON] Text :> Post '[JSON] ()
type RouteAliasGetAll     = "get" :> "all" :> Get '[JSON] [Text]
type RouteAliasSetDefault = "setDefault"   :> ReqBody '[JSON] Text :> Post '[JSON] ()

-- episode

type RouteEpisodeNew = "new" :> ReqBody '[JSON] EpisodeNew :> Post '[JSON] ()

data EpisodeNew = EpisodeNew
  { newCustomIndex :: Text
  , newTitle       :: Text
  , newDate        :: Text
  } deriving (Read, Show, Generic)

instance FromJSON EpisodeNew
instance ToJSON EpisodeNew

-- podcast

type RoutePodcastNew = "new" :> ReqBody '[JSON] Text :> Post '[JSON] ()
type RoutePodcastGet = Capture "podcast_id" Text :> Get '[JSON] Podcast

routes :: Proxy Routes
routes = Proxy

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance MimeRender XML Lazy.ByteString where
  mimeRender _ = id

instance MimeUnrender XML Lazy.ByteString where
  mimeUnrender _ bs = pure bs

convertToFilename :: Text -> Text
convertToFilename str =
  let map = [ ("Ä", "A")
            , ("Ö", "O")
            , ("Ü", "U")
            , ("ß", "SS")
            , ("?", "_")
            , ("!", "_")
            , (",", "_")
            , (";", "_")
            , (":", "_")
            , ("'", "_")
            , ("=", "_")
            , ("<", "_")
            , (">", "_")
            , ("/", "_")
            , ("\\", "_")
            , ("\"", "_")
            , ("&", "_")
            , ("@", "_")
            , ("%", "_")
            , ("+", "_")
            , ("*", "_")
            , ("$", "_")
            , (" ", "_")
            , ("(", "_")
            , (")", "_")
            , ("É", "E")
            , ("Á", "A")
            , ("Í", "I")
            , ("È", "E")
            , ("À", "A")
            , ("Ì", "I")
            ]
      acc str' (s, t) = replace s t str'
  in  foldl' acc str map

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `div` 60) `mod` 60
      hours = d `div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds
