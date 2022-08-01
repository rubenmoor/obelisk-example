{-# LANGUAGE TemplateHaskell #-}
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
import           Control.Monad.Reader     (MonadReader, asks)
import           Data.Either              (Either (Right))
import           Data.Function            (($))
import           Data.Functor             ((<&>), Functor (fmap), (<$>))
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
import           GHC.Num                  (Num ((*)))
import           GHC.Real                 (Integral (div, mod))
import           Common.Model                    (Episode (..), Platform (..),
                                           PlatformName (..), Podcast (..),
                                           Rank (..))
import           Obelisk.Generated.Static (static)
import           Obelisk.Route.Frontend   (Routed (askRoute))
import           Reflex.Dom               (fanEither, DomBuilder, MonadHold,
                                           PostBuild (getPostBuild), Prerender,
                                           Reflex (Dynamic), blank, dyn_, el,
                                           elAttr, elClass, ffor, text,
                                           widgetHold_, zipDyn, (=:))
import           Common.Route                    (EpisodeSlug, PodcastIdentifier (unPodcastIdentifier))
import           Shared                   (iFa)
import           State                    (Session (..),
                                           State (..))
import           Text.Printf              (printf)
import           Text.Regex.TDFA          ((=~))
import qualified Text.URI                 as URI
import Data.Tuple (fst)
import Control.Category (Category ((.)))

pagePodcastView
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , MonadHold t m
  , MonadReader (Dynamic t State) m
  , PostBuild t m
  , Prerender t m
  , Routed t (PodcastIdentifier, Maybe EpisodeSlug) m
  ) => m ()
pagePodcastView = do
  dynRoute <- askRoute
  let dynPodcastId = unPodcastIdentifier . fst <$> dynRoute
  dynSession <- asks $ fmap stSession
  dyn_ $ ffor (zipDyn dynSession dynPodcastId) $ \case
    (SessionAnon, _) -> blank
    (SessionUser SessionData{..}, pId) -> case Map.lookup pId sdClearances of
      Just rank | rank >= RankAdmin -> iFa "fas fa-edit"
      _                             -> blank
  ePb <- getPostBuild
  (evFailure, evPodcast) <- fanEither <$>
    request (getPodcast (Right <$> dynPodcastId) ePb)
  widgetHold_ blank $ evPodcast <&> \(Podcast{..}, platforms, episodes) -> do
    elClass "div" "row" $
      elClass "div" "col-12" $ do
        el "h2" $ text podcastTitle
        el "h4" $ text podcastAuthors
    elClass "div" "row" $ do
      elAttr "div" ("class" =: "col-8") $ do
        forM_ platforms $ \Platform{..} -> do
          let imgSrc = case platformName of
                PlatformSpotify  -> $(static "spotify.png")
                PlatformTelegram -> $(static "telegram.png")
                PlatformItunes   -> $(static "itunes.png")
                PlatformYoutube  -> $(static "youtube.png")
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
