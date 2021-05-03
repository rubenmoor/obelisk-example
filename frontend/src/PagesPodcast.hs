{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PagesPodcast
  ( pagePodcastView
  ) where

import           Client                   (getPodcast, request)
import           Common.Auth              (SessionData (..))
import           Control.Monad            (forM_)
import           Control.Monad.Fix        (MonadFix)
import           Control.Monad.Reader     (MonadReader, asks)
import           Data.Either              (Either (Right))
import           Data.Function            (($))
import           Data.Functor             (Functor (fmap), (<$>))
import           Data.Int                 (Int)
import qualified Data.Map                 as Map
import           Data.Maybe               (Maybe (Just))
import           Data.Monoid              (Monoid (mempty))
import           Data.Ord                 (Ord ((<), (>=)))
import           Data.Semigroup           (Semigroup ((<>)))
import           Data.Text                (Text, toLower)
import qualified Data.Text                as Text
import           Data.Time                (formatTime)
import           Data.Time.Format         (defaultTimeLocale)
import           Data.Witherable          (Filterable (mapMaybe))
import           GHC.Num                  (Num ((*)))
import           GHC.Real                 (Integral (div, mod))
import           Model                    (Episode (..), Platform (..),
                                           PlatformName (..), Podcast (..),
                                           Rank (..))
import           Obelisk.Generated.Static (static)
import           Obelisk.Route.Frontend   (R, RouteToUrl, Routed (askRoute),
                                           RoutedT, SetRoute)
import           Reflex.Dom               (DomBuilder, EventWriter, MonadHold,
                                           PostBuild (getPostBuild), Prerender,
                                           Reflex (Dynamic, Event, never),
                                           blank, dyn_, el, elAttr, elClass,
                                           ffor, switchHold, text, widgetHold_,
                                           zipDyn, (=:))
import           Route                    (FrontendRoute, PodcastIdentifier (unPodcastIdentifier))
import           Servant.Reflex           (reqSuccess)
import           Shared                   (iFa)
import           State                    (EStateUpdate, Session (..),
                                           State (..))
import Text.Regex.TDFA ((=~))
import           Text.Printf              (printf)
import qualified Text.URI                 as URI

pagePodcastView
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadHold t m
  , MonadFix m
  , MonadReader (Dynamic t State) m
  , PostBuild t m
  , Prerender js t m
  , Routed t PodcastIdentifier m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  ) => m ()
pagePodcastView = do
  dynPodcastId <- fmap unPodcastIdentifier <$> askRoute
  dynSession <- asks $ fmap stSession
  dyn_ $ ffor (zipDyn dynSession dynPodcastId) $ \case
    (SessionAnon, _) -> blank
    (SessionUser SessionData{..}, pId) -> case Map.lookup pId sdClearances of
      Just rank | rank >= RankAdmin -> iFa "fas fa-edit"
      _                             -> blank
  ePb <- getPostBuild
  ePodcast <- mapMaybe reqSuccess <$>
    request (getPodcast (Right <$> dynPodcastId) ePb)
  widgetHold_ blank $ ffor ePodcast $ \(Podcast{..}, platforms, episodes) -> do
    elClass "div" "row" $
      elClass "div" "col-12" $ do
        el "h2" $ text podcastTitle
        el "h4" $ text podcastAuthors
    elClass "div" "row" $ do
      elAttr "div" ("class" =: "col-8") $ do
        forM_ platforms $ \Platform{..} -> do
          let imgSrc = case platformName of
                PlatformSpotify  -> static @"spotify.png"
                PlatformTelegram -> static @"telegram.png"
                PlatformItunes   -> static @"itunes.png"
                PlatformYoutube  -> static @"youtube.png"
          elAttr "a" ("href" =: URI.render platformLink) $
            elAttr "img" ("src" =: imgSrc) blank
      elAttr "div" ("class" =: "col-4") $ text podcastDescription
      elAttr "div" ("class" =: "col-8") $ forM_ episodes $ \Episode{..} -> do
        el "div" $ do
          let audioSrc = mkAudioFilePath podcastIdentifier episodeSlug episodeFtExtension
          el "h3" $ text $ "#" <> episodeCustomIndex <> "  " <> toLower episodeTitle
          el "span" $ text $ Text.pack $ formatTime defaultTimeLocale "%F" episodePubdate
          -- TODO: introduce pagination and activate preload
          elAttr "audio" ("controls" =: "controls" <> "preload" =: "none") $
            elAttr "source" ("src" =: audioSrc) blank
          elAttr "div" mempty $ text $ "Duration: " <> formatDuration episodeDuration
          elAttr "span" mempty $ do
            text "("
            elAttr "a" (   "href" =: audioSrc
                        <> "title" =: episodeFtExtension
                        <> "download" =: episodeSlug
                       ) $ text "download"
            text ")"
          elAttr "div" mempty $ makeClickableLinks episodeDescriptionLong

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `div` 60) `mod` 60
      hours = d `div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds

mkAudioFilePath :: Text -> Text -> Text -> Text
mkAudioFilePath podcastIdentifier slug ext =
  "/media/" <> podcastIdentifier <> "/" <> slug <> ext

makeClickableLinks
  :: DomBuilder t m
  => Text
  -> m ()
makeClickableLinks str =
    let (prefix, _ :: Text, suffix, groups) =
          str =~ ("([[:space:]]|\\`)https?://(]|[-a-zA-Z0-9._~%!*\'();:@&=+$,/?#[])+" :: Text)
          -- raw
          -- str =~ ([r|([[:space:]]|\`)https?://(]|[-a-zA-Z0-9._~%!*'();:@&=+$,/?#[])+|] :: Text)
    in case groups of
         []       -> text str
         ws:[url] -> do text $ prefix <> ws
                        toLink url
                        makeClickableLinks suffix
         _        -> text str -- impossible
  where
    toLink s =
      let inner = if Text.length s < 41 then s else Text.take 37 s <> "..."
      in  elAttr "a" ("href" =: s) $ text inner
