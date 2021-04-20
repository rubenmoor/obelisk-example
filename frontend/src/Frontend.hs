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

import qualified Data.Text.Lazy.Encoding     as Lazy
import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (pattern (:/), R)
import           Clay                        (Center (center), Cursor (cursor),
                                              None (none), absolute, alignItems,
                                              backgroundColor, block, color,
                                              compact, darkgray, display, em,
                                              fontSizeCustom, inlineFlex,
                                              justifyContent, marginLeft,
                                              marginRight, minHeight, padding,
                                              pointer, position, px, renderWith,
                                              right, smaller, textAlign, white,
                                              zIndex)
import           Client                      (getAuthData, postPodcastNew,
                                              request)
import           Common.Auth                 (UserInfo (..))
import           Control.Applicative         (Applicative (pure))
import           Control.Category            (Category ((.)))
import           Control.Monad               (Monad ((>>=)), when)
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (MonadReader, ReaderT (runReaderT),
                                              asks)
import qualified Data.Aeson                  as Aeson
import           Data.Bool                   (Bool (False, True), bool, not)
import           Data.Default                (Default (def))
import           Data.Either                 (Either (..))
import           Data.Function               (const, flip, ($))
import           Data.Functor                (Functor (fmap), ($>), (<$>))
import           Data.Maybe                  (Maybe (..), fromMaybe, maybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, unwords)
import qualified Data.Text.Lazy              as Lazy
import           Data.Tuple                  (fst)
import           Data.Witherable             (Filterable (mapMaybe))
import           GHCJS.DOM                   (currentWindowUnchecked)
import           GHCJS.DOM.Storage           (getItem, setItem)
import           GHCJS.DOM.Window            (getLocalStorage)
import           Language.Javascript.JSaddle (liftJSM)
import           MediaQuery                  (ResponsiveClass (rcClass),
                                              onDesktopBorder,
                                              onDesktopDisplayImportant,
                                              onDesktopDisplayNone,
                                              onDesktopMkOverlay,
                                              onMobileAtBottom,
                                              onMobileDisplayNone,
                                              onMobileFontBig, onMobileHeight80,
                                              onMobileMkOverlay,
                                              onMobileWidthAuto, respClass,
                                              respClasses)
import           Model                       (Alias (aliasName))
import           Obelisk.Route.Frontend      (RouteToUrl, RoutedT, SetRoute,
                                              mapRoutedT, routeLink, subRoute_)
import           PagesUser                   (pageAliasRename, pageAliasSelect,
                                              pageLogin, pageRegister)
import           Reflex.Dom                  (elAttr, text, el, blank, DomBuilder (inputElement),
                                              EventName (Click),
                                              EventWriter (tellEvent),
                                              HasDomEvent (domEvent), MonadHold,
                                              PerformEvent (..), PostBuild,
                                              Prerender (prerender),
                                              Reflex (Dynamic, never, updated),
                                              dyn, dyn_, elAttr', elDynAttr',
                                              ffor, foldDyn, foldDynMaybe,
                                              leftmost, prerender_,
                                              runEventWriterT, switchHold,
                                              tailE, widgetHold_, (.~), (=:))
import           Route                       (FrontendRoute (..))
import           Servant.Common.Req          (reqSuccess)
import           Shared                      (btnSend, cssGeneral, elLabelInput,
                                              iFa, iFa', navBorder, style)
import           State                       (EStateUpdate (..), Session (..),
                                              State (..))
import PagesPodcast (pagePodcastView)

pageHome
  :: forall t (m :: * -> *)
  .  DomBuilder t m
  => m ()
pageHome = pure ()

pageSettings
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , MonadReader (Dynamic t State) m
  , PostBuild t m
  , Prerender js t m
  ) => m ()
pageSettings = do
  authData <- asks getAuthData
  dynSession <- asks $ fmap stSession
  dyn_ $ ffor dynSession $ \case
    SessionAnon -> blank
    SessionUser _ UserInfo{..} -> when uiIsSiteAdmin $ do
      el "h3" $ text "Create new podcast"
      (mStr, _) <- elLabelInput def "Podcast identifier" "podcastidentifier"
      eSend <- btnSend $ text "Send"
      let eStr = maybe (Left "Cannot be empty") Right <$> mStr
      response <- request (postPodcastNew authData eStr eSend)
      let eSuccess = mapMaybe reqSuccess response
      -- setRoute view podcast
      blank
  pure ()

navigation
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , EventWriter t EStateUpdate m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , MonadReader (Dynamic t State) m
  ) => m ()
navigation = do
  let cssNav = do
        -- textAlign center
        backgroundColor white
        navBorder
        cursor pointer
        minHeight $ px 60
      divNavBar = elAttr "div" ("class" =: unwords
        [ "row"
        , rcClass onMobileAtBottom
        , rcClass onMobileFontBig
        ] <> style (do textAlign center
                       backgroundColor white
                       zIndex 2
                   ))
  dynSession <- asks $ fmap stSession
  divNavBar $ mdo
    let navButtonAttrs = ffor displayNav $ \d ->
             "class" =: unwords [ "col-2"
                                , rcClass onMobileHeight80
                                , rcClass onDesktopDisplayImportant
                                ]
          <> style (do cssNav
                       display d
                       zIndex 2
                   )
        spanNavBtn = fmap fst . elDynAttr' "span" navButtonAttrs
    eBtns <- dyn $ ffor dynSession $ \case
      SessionAnon -> do
        elLogin <- routeLink (FrontendRoute_Login :/ ()) $
          spanNavBtn $ text "Login"
        elRegister <- routeLink (FrontendRoute_Register :/ ()) $
          spanNavBtn $ text "Register"
        pure $ leftmost $ domEvent Click <$> [elLogin, elRegister]
      SessionUser _ ui -> do
        elSettings <- routeLink (FrontendRoute_Settings :/ ()) $
          spanNavBtn $ text "Settings"
        elLogout <- spanNavBtn $ do
          let alias = aliasName $ uiAlias ui
              css = style $ do
                color darkgray
                fontSizeCustom smaller
                marginRight $ em 0.5
          elAttr "span" css $ text alias
          text "Logout"
        let eLogout = domEvent Click elLogout
        tellEvent $ eLogout $> EStateUpdate (\s -> s { stSession = SessionAnon })
        pure $ leftmost [eLogout, domEvent Click elSettings]
    eExit <- switchHold never eBtns

    let spanNavBtnHome = fmap fst . elAttr' "span"
          ("class" =: unwords [ "col-2"
                              , rcClass onMobileWidthAuto
                              , rcClass $ onDesktopBorder navBorder
                              ]
          <> style (do minHeight $ px 60
                       display inlineFlex
                       alignItems center
                       justifyContent center
                   ))
    elHome <- routeLink (FrontendRoute_Main :/ ()) $
      spanNavBtnHome $ do
        iFa "fas fa-home"
        elAttr "span" (respClass onMobileDisplayNone <> style (marginLeft $ em 0.5)) $ text "Home"
        blank

    let spanNavBars =
          elAttr "span" (
               respClasses [onDesktopDisplayNone]
            <> style (do position absolute
                         right (px 0)
                         padding (px 0) (px 8) (px 0) (px 8)
                         ))
    elBars <- spanNavBars $ iFa' "fas fa-bars"

    let eHome = domEvent Click elHome
        eToggle = domEvent Click elBars
        toggleFunc True  s     = Just $ not s -- toggle btn always toggles
        toggleFunc False True  = Just False -- other btns only hide
        toggleFunc False False = Nothing
    dynToggle <- foldDynMaybe toggleFunc False $
      leftmost [ eToggle $> True
               , eExit $> False
               , eHome $> False
               ]
    let displayNav = bool none block <$> dynToggle
    blank

htmlBody
  :: forall t js (m :: * -> *)
  . (ObeliskWidget js t (R FrontendRoute) m)
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
  eUpdated <- tailE $ updated dynState
  prerender_ blank $ performEvent_ $ ffor eUpdated $ \st -> do
    let str = Lazy.toStrict $ Lazy.decodeUtf8 $ Aeson.encode st
    liftJSM (currentWindowUnchecked >>= getLocalStorage >>= setState str)

  (_, eStateUpdate) <- mapRoutedT (flip runReaderT dynState . runEventWriterT) $ do
    navigation
    subRoute_ $ \case
      FrontendRoute_Main        -> pageHome
      FrontendRoute_Register    -> pageRegister
      FrontendRoute_Login       -> pageLogin
      FrontendRoute_AliasSelect -> pageAliasSelect
      FrontendRoute_AliasRename -> pageAliasRename
      FrontendRoute_Settings    -> pageSettings
      FrontendRoute_Podcast     -> pagePodcastView
  blank
  where
    loadingScreen =
      let cls = respClasses
            [ onMobileMkOverlay
            , onDesktopMkOverlay
            ]
          css = do backgroundColor white
                   padding (px 24) (px 24) (px 24) (px 24)
                   position absolute
      in  elAttr "div" (cls <> style css) $ do
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
  :: forall t js (m :: * -> *)
  .  (ObeliskWidget js t (R FrontendRoute) m)
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
  elAttr "link" ( "rel" =: "preconnect"
               <> "href" =: "https://fonts.gstatic.com"
                ) blank
  elAttr "link" ( "href" =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
               <> "rel" =: "stylesheet"
                ) blank
  -- Font Awesome 5.13 free content -->
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: static @"FontAwesome/css/all.min.css"
                ) blank
  el "title" $ text "serendipity.works"
  el "style" $ text $ Lazy.toStrict $ renderWith compact [] cssGeneral

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHead
  , _frontend_body = htmlBody
  }
