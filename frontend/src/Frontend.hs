{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Frontend where

import Data.Tuple (fst)
import           Client                      (getAuthData, postPodcastNew,
                                              request)
import           Common.Auth                 (SessionData (..))
import           Control.Applicative         (Applicative (pure))
import           Control.Category            (Category ((.)))
import           Control.Monad               (Monad ((>>=)), when)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader, ReaderT (runReaderT),
                                              asks)
import           Data.Generics.Product  (field)
import qualified Data.Aeson                  as Aeson
import           Data.Default                (Default (def))
import           Data.Either                 (Either (..))
import           Data.Function               (const, flip, ($))
import           Data.Functor                ((<&>), Functor (fmap), ($>), (<$>))
import           Data.Maybe                  (Maybe (..), fromMaybe, maybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as Lazy
import qualified Data.Text.Lazy.Encoding     as Lazy
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           Language.Javascript.JSaddle (liftJSM)
import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (R)
import           Obelisk.Route.Frontend      (RouteToUrl, RoutedT, SetRoute,
                                              mapRoutedT, subRoute_, askRoute)
import           Pages.Podcast               (pagePodcastView)
import           Reflex.Dom                  (constDyn, elClass, fanEither, elDynClass', DomBuilder, EventName (Click),
                                              EventWriter,
                                              HasDomEvent (domEvent), MonadHold,
                                              PerformEvent (..), PostBuild,
                                              Prerender (prerender),
                                              Reflex (Dynamic, never, updated),
                                              blank, dyn, dyn_, el, elAttr,
                                              elAttr',
                                              foldDyn, foldDynMaybe, leftmost,
                                              prerender_, runEventWriterT,
                                              switchHold, tailE, text,
                                              widgetHold_, (=:))
import           Common.Route                 (FrontendRoute (..), PodcastIdentifier (..))
import           Elements.Shared              (elPage)
import           Elements.Common              (btnSend, elLabelInput, iFa)
import           State                        (EStateUpdate (..), Session (..),
                                              State (..), updateState)
import Control.Lens.Setter ((.~))

pageHome
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Prerender t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , MonadReader (Dynamic t State) m
  ) => m ()
pageHome = blank

pageSettings
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Prerender t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , MonadReader (Dynamic t State) m
  ) => m ()
pageSettings = do
    authData <- asks getAuthData
    dynSession <- asks $ fmap stSession
    dyn_ $ dynSession <&> \case
      SessionAnon -> blank
      SessionUser SessionData{..} -> when sdIsSiteAdmin $ do
        el "h3" $ text "Create new podcast"
        (mStr, _) <- elLabelInput def "Podcast identifier" "podcastidentifier"
        eSend <- btnSend $ text "Send"
        let eStr = maybe (Left "Cannot be empty") Right <$> mStr
        (_, evSuccess) <- fanEither <$> request (postPodcastNew authData eStr eSend)
        -- TODO
        -- setRoute view podcast
        blank

htmlBody
  :: forall t (m :: * -> *)
  . (ObeliskWidget t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
htmlBody = mdo

  -- get application state on startup
  -- ePostBuild <- getPostBuild
  let key = "state" :: Text
      getState s = getItem s key
      setState d s = setItem s key d
  dynLoadState <- prerender (pure $ EStateUpdate $ const def) $ do
    -- performEvent $ ePostBuild $>
    mStr <- liftJSM (currentWindowUnchecked >>= getLocalStorage >>= getState)
    let mState = mStr >>= Aeson.decode . Lazy.encodeUtf8 . Lazy.fromStrict
    pure $ EStateUpdate $ const $ fromMaybe def mState

  let eLoaded = updated dynLoadState
  widgetHold_ loadingScreen $ eLoaded $> blank

  dynState <- foldDyn unEStateUpdate def $ leftmost [eLoaded, eStateUpdate]

  -- TODO persist application state on visibility change (when hidden)
  evUpdated <- tailE $ updated dynState
  prerender_ blank $ performEvent_ $ evUpdated <&> \st -> do
    let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
    liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

  (_, eStateUpdate) <- mapRoutedT (flip runReaderT dynState . runEventWriterT) $ do
    dyn_ $ dynState <&> \State{..} -> case stMsg of
      Nothing -> pure ()
      Just str -> divMsgOverlay $ el "span" $ text str
    subRoute_ $ \case
      FrontendRoute_Main        -> elPage "Home" pageHome
      FrontendRoute_Settings    -> elPage "Settings" pageSettings
      FrontendRoute_Podcast     -> do
          dynRoute <- askRoute
          let dynPodcastId = unPodcastIdentifier . fst <$> dynRoute
          elPage dynPodcastId $ pagePodcastView dynPodcastId
  blank
  where
    loadingScreen =
      elAttr "div" ("class" =: "mkOverlay") $ do
        iFa "fas fa-spinner fa-spin"
        text " Loading ..."

    -- TODO: replace with home component
    -- el "h1" $ text "Create new episode"
    -- pb <- getPostBuild
    -- postBuildTime <- performEvent $ pb $> liftIO getCurrentTime
    -- let today = Text.pack . formatTime defaultTimeLocale "%F" <$> postBuildTime
    -- (date, _) <- elLabelInput (def & inputElementConfig_setValue .~ today)
    --                      "Episode date: " "date"
    -- (customIndex, _) <- elLabelInput def "Custom index: " "customIndex"
    -- (title, _) <- elLabelInput def "Episode title: " "title"

    -- text "Title: "
    -- el "br" blank

    -- -- dynamic title
    -- let cfg = do
    --       mc <- customIndex
    --       mt <- title
    --       let mTitle = do
    --             c <- mc
    --             t <- mt
    --             pure (Map.empty, "#" <> c <> " " <> t)
    --       pure $ fromMaybe (style $ fontStyle italic, "empty") mTitle
    -- elDynAttr "span" (fst <$> cfg) $ dynText (snd <$> cfg)

    -- el "br" blank
    -- text "Episode slug (no special chars allowed): "
    -- el "br" blank
    -- dynText $ do mt <- title
    --              let str = (convertToFilename . toUpper . fromMaybe "") mt
    --              md <- date
    --              pure $ fromMaybe "" md <> "-" <> str

    -- el "br" blank
    -- sendButton <- button "send"

    -- let eitherEpisodeNew = do
    --       mCustomIndex <- customIndex
    --       mTitle       <- title
    --       mDate        <- date
    --       let mEpisodeNew = EpisodeNew <$> mCustomIndex <*> mTitle <*> mDate
    --       pure $ maybe (Left "all fields are required") Right mEpisodeNew
    -- -- with auth
    -- -- res <- postEpisodeNew (constDyn $ Left "no jwt") eitherEpisodeNew sendButton
    -- elAttr "p" (style $ color red) $ prerender_ (text "loading") $ do
    --   -- TODO: make client aware of currently selected show
    --   res <- lift $ postEpisodeNew (constDyn $ Right "fullserendipity")
    --                                (constDyn $ Left "no jwt")
    --                                eitherEpisodeNew
    --                                sendButton
    --   let err = mapMaybe reqFailure res
    --   holdDyn "" err >>= dynText

    -- el "h1" $ text "Welcome to Obelisk!"
    -- el "p" $ text "foooooo"
    -- blank


htmlHead
  :: forall t (m :: * -> *)
  .  (ObeliskWidget t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
htmlHead = do
  elAttr "meta" ( "content" =: "text/html;charset=utf-8"
               <> "httpequiv" =: "content-type"
                ) blank
  elAttr "meta" ( "content" =: "utf-8"
               <> "httpequiv" =: "encoding"
                ) blank
  elAttr "meta" ( "name" =: "viewport"
               <> "content" =: "width=device-width, initial-scale=1.0"
                ) blank
  elAttr "link" ( "rel" =: "icon"
               <> "type" =: "image/x-icon"
               <> "href" =: $(static "favicon.ico")
                ) blank
  elAttr "link" ( "rel" =: "preconnect"
               <> "href" =: "https://fonts.gstatic.com"
                ) blank
  elAttr "link" ( "href" =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
               <> "rel" =: "stylesheet"
                ) blank
  -- Font Awesome 5.13 free content -->
  -- static
  -- elAttr "link" ( "rel" =: "stylesheet"
  --              <> "href" =: static @"FontAwesome/css/all.min.css"
  --               ) blank
  -- web
  elAttr "script" ( "src" =: "https://kit.fontawesome.com/63887ea353.js" ) blank
  el "title" $ text "serendipity.works"
  elAttr "link" ( "href" =: $(static "styles.css")
               <> "rel" =: "stylesheet"
                ) blank
  -- el "style" $ text $ Lazy.toStrict $ renderWith compact [] cssGeneral

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHead
  , _frontend_body = htmlBody
  }

divMsgOverlay
  :: forall t (m :: * -> *) a.
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  ) => m a -> m a
divMsgOverlay inner =
  let spanClose = elAttr' "span" ("style" =: "float:right") $ iFa "fas fa-times"
      cls = "class" =: "msgOverlay"
  in  elAttr "div" cls $ do
    (elClose, _) <- spanClose
    let eClose = domEvent Click elClose
    updateState $ eClose $> (field @"stMsg" .~ Nothing)
    inner
