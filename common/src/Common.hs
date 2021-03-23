{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Common where

import           Control.Applicative      (Applicative (pure))
import           Control.Category         (Category (id))
import qualified Data.ByteString.Lazy     as Lazy
import           Network.HTTP.Media       ((//), (/:))
import           Servant.API              ((:<|>) (..), (:>), Get)
import           Servant.API.ContentTypes (Accept (..), MimeRender (..),
                                           MimeUnrender (..))
import Data.Data (Proxy(Proxy))

type Routes =
       RoutesHandlers

type RoutesHandlers =
  RouteFeed

type RouteFeed = "feed.xml" :> Get '[XML] Lazy.ByteString

routes :: Proxy Routes
routes = Proxy

data XML

instance Accept XML where
  contentType _ = "application" // "xml" /: ("charset", "utf-8")

instance MimeRender XML Lazy.ByteString where
  mimeRender _ = id

instance MimeUnrender XML Lazy.ByteString where
  mimeUnrender _ bs = pure bs
