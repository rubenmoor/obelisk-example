{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PagesPodcast
  ( pagePodcastView
  ) where

import           Common.Auth            (UserInfo (..))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader, asks)
import qualified Data.Map               as Map
import           Model                  (Podcast (..), Rank (..))
import           Obelisk.Route.Frontend (R, RouteToUrl, Routed (askRoute),
                                         RoutedT, SetRoute)
import           Reflex.Dom             (widgetHold_, switchHold, el, elClass, text, elAttr, (=:), PostBuild(getPostBuild),  DomBuilder, EventWriter, MonadHold,
                                         Prerender, Reflex(Event, never, Dynamic), blank,
                                         dyn_, ffor, zipDyn)
import           Route                  (FrontendRoute,
                                         PodcastIdentifier (unPodcastIdentifier))
import           State                  (EStateUpdate, Session (..), State (..))
import Shared (iFa)
import Data.Witherable (Filterable(mapMaybe))
import Servant.Reflex (reqSuccess)
import Client (getPodcast, request)

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
    (SessionUser _ UserInfo{..}, pId) -> case Map.lookup pId uiClearances of
      Just r | r >= RankAdmin -> iFa "fas fa-edit"
      _                       -> blank
  ePb <- getPostBuild
  ePodcast <- mapMaybe reqSuccess <$>
    request (getPodcast (Right <$> dynPodcastId) ePb)
  widgetHold_ blank $ ffor ePodcast $ \Podcast{..} -> do
    elClass "div" "row" $
      elClass "div" "col-12" $ do
        el "h2" $ text podcastTitle
        el "h4" $ text podcastAuthors
        el "span" $ text podcastDescription
    elAttr "div" ("class" =: "col-8") $ text "platform links"
  blank
