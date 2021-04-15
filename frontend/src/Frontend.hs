{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Frontend where

import qualified Data.Text.Lazy.Encoding     as Lazy
import           Obelisk.Frontend            (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static    (static)
import           Obelisk.Route               (pattern (:/), R)

import           Reflex.Dom.Core             (blank, el, elAttr, text)

import           Clay                        (Center (center), None (none),
                                              absolute, backgroundColor, block,
                                              compact, display, padding,
                                              position, px, renderWith, right,
                                              textAlign, white, zIndex)
import           Control.Applicative         (Applicative (pure))
import           Control.Category            (Category ((.)))
import           Control.Monad               (Monad ((>>=)))
import qualified Data.Aeson                  as Aeson
import           Data.Bool                   (Bool (False, True), bool, not)
import           Data.Default                (Default (def))
import           Data.Function               (const, ($))
import           Data.Functor                (void, Functor (fmap), ($>), (<$>))
import           Data.Maybe                  (Maybe (..), fromMaybe, maybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, unwords)
import qualified Data.Text.Lazy              as Lazy
import           Data.Tuple                  (fst)
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
import           Obelisk.Route.Frontend      (RoutedT, mapRoutedT, routeLink,
                                              subRoute_)
import           PagesUser                   (pageLogin, pageRegister, pageSelectAlias)
import           Reflex.Dom                  (tailE, widgetHold_, prerender_, DomBuilder (inputElement),
                                              EventName (Click),
                                              HasDomEvent (domEvent),
                                              PerformEvent (..),
                                              Prerender (prerender),
                                              Reflex (never, updated), dyn,
                                              dyn_, elAttr', elDynAttr', ffor,
                                              foldDyn, foldDynMaybe, leftmost,
                                              runEventWriterT, switchHold, (.~),
                                              (=:))
import           Route                       (FrontendRoute (..))
import           Shared                      (cssGeneral, iFa, iFa', navBorder,
                                              style)
import           State                       (EStateUpdate (..), Session (..),
                                              State (..))

pageHome
  :: forall t (m :: * -> *)
  .  DomBuilder t m
  =>  m ()
pageHome = pure ()

navigation dynSession = do
  let cssNav = do
        textAlign center
        backgroundColor white
        navBorder
      divNavBar = elAttr "div" ("class" =: unwords
        [ "row"
        , rcClass onMobileAtBottom
        , rcClass onMobileFontBig
        ] <> style (do textAlign center
                       backgroundColor white
                       zIndex 2
                   ))
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
      SessionUser _ _ -> do
        elLogout <- spanNavBtn $ text "Logout"
        pure $ domEvent Click elLogout
    eExit <- switchHold never eBtns

    let spanNavBtnHome = fmap fst . elAttr' "span"
          ("class" =: unwords [ "col-2"
                              , rcClass onMobileWidthAuto
                              , rcClass $ onDesktopBorder navBorder
                              ])
    elHome <- routeLink (FrontendRoute_Main :/ ()) $
      spanNavBtnHome $ do
        iFa "fas fa-home"
        elAttr "span" (respClass onMobileDisplayNone) $ text "Home"
        blank

    let spanNavBars = elAttr "span" (respClasses [onDesktopDisplayNone]
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

  navigation $ stSession <$> dynState
  (_, eStateUpdate) <- mapRoutedT runEventWriterT $
    subRoute_ $ \case
      FrontendRoute_Main        -> pageHome
      FrontendRoute_Register    -> pageRegister
      FrontendRoute_Login       -> pageLogin
      FrontendRoute_SelectAlias -> pageSelectAlias $ stSession <$> dynState
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
