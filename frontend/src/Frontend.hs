{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE PatternSynonyms #-}

module Frontend where

import qualified Data.Map                  as Map
import qualified Data.Text                 as Text

import           Obelisk.Frontend          (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static  (static)
import           Obelisk.Route             (pattern (:/), R)

import           Reflex.Dom.Core           (blank, button, el, elAttr,
                                            inputElementConfig_setValue, text)

import           Clay                      (Center (center), None (none),
                                            absolute, backgroundColor, block,
                                            color, compact, display, fontStyle,
                                            italic, padding, position, px, red,
                                            renderWith, right, textAlign, white,
                                            zIndex)
import           Client                    (postEpisodeNew)
import           Common                    (EpisodeNew (..), convertToFilename)
import           Control.Applicative       (Applicative (pure, (<*>)))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)))
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Bool                 (Bool (False, True), bool, not)
import           Data.Default              (Default (def))
import           Data.Either               (Either (..))
import           Data.Function             (($), (&))
import           Data.Functor              (Functor (fmap), ($>), (<$>))
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (toUpper, unwords)
import qualified Data.Text.Lazy            as Lazy
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           Data.Tuple                (fst, snd)
import           Data.Witherable           (mapMaybe)
import           MediaQuery                (ResponsiveClass (rcClass),
                                            onDesktopBorder,
                                            onDesktopDisplayImportant,
                                            onDesktopDisplayNone,
                                            onMobileAtBottom,
                                            onMobileDisplayNone,
                                            onMobileFontBig, onMobileHeight80,
                                            onMobileWidthAuto, respClass,
                                            respClasses)
import           Obelisk.Route.Frontend    (routeLink, RoutedT, subRoute_)
import           PagesUser                 (pageRegister)
import           Reflex.Dom                (DomBuilder (inputElement),
                                            EventName (Click),
                                            HasDomEvent (domEvent),
                                            MonadHold (holdDyn),
                                            PerformEvent (performEvent),
                                            PostBuild (getPostBuild), constDyn,
                                            dynText, elAttr', elDynAttr,
                                            elDynAttr', ffor, foldDynMaybe,
                                            leftmost, prerender_, (.~), (=:))
import           Route                     (FrontendRoute (..))
import           Servant.Common.Req        (reqFailure)
import Shared (cssGeneral, elLabelInput, iFa', iFa, style, navBorder)


pageHome
  :: forall t js s (m :: * -> *)
  .  DomBuilder t m
  =>  m ()
pageHome = pure ()

htmlBody
  :: forall t js (m :: * -> *)
  . (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
htmlBody = do

  -- navigation
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
  (eLogin, eRegister) <- divNavBar $ mdo
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
    elLogin <- spanNavBtn $ text "Login"
    elRegister <- routeLink (FrontendRoute_Register :/ ()) $ spanNavBtn $ text "Register"

    let spanNavBtnHome = fmap fst . elAttr' "span"
          ("class" =: unwords [ "col-2"
                              , rcClass onMobileWidthAuto
                              , rcClass $ onDesktopBorder navBorder
                              ])
    elHome <- spanNavBtnHome $ do
      iFa "fas fa-home"
      elAttr "span" (respClass onMobileDisplayNone) $ text "Home"

    let spanNavBars = elAttr "span" (respClasses [onDesktopDisplayNone]
                                   <> style (do position absolute
                                                right (px 0)
                                                padding (px 0) (px 8) (px 0) (px 8)
                                            ))
    elBars <- spanNavBars $ iFa' "fas fa-bars"

    let eLogin = domEvent Click elLogin
        eRegister = domEvent Click elRegister
    let eToggle = domEvent Click elBars
        toggleFunc True  s     = Just $ not s -- toggle btn always toggles
        toggleFunc False True  = Just False -- other btns only hide
        toggleFunc False False = Nothing
    dynToggle <- foldDynMaybe toggleFunc False $
      leftmost [eToggle $> True, eLogin $> False, eRegister $> False]
    let displayNav = bool none block <$> dynToggle
    pure (eLogin, eRegister)


  subRoute_ $ \case
    FrontendRoute_Main     -> pageHome
    FrontendRoute_Register -> pageRegister

  el "h1" $ text "Create new episode"
  pb <- getPostBuild
  postBuildTime <- performEvent $ pb $> liftIO getCurrentTime
  let today = Text.pack . formatTime defaultTimeLocale "%F" <$> postBuildTime
  (date, _) <- elLabelInput (def & inputElementConfig_setValue .~ today)
                       "Episode date: " "date"
  (customIndex, _) <- elLabelInput def "Custom index: " "customIndex"
  (title, _) <- elLabelInput def "Episode title: " "title"

  text "Title: "
  el "br" blank

  -- dynamic title
  let cfg = do
        mc <- customIndex
        mt <- title
        let mTitle = do
              c <- mc
              t <- mt
              pure (Map.empty, "#" <> c <> " " <> t)
        pure $ fromMaybe (style $ fontStyle italic, "empty") mTitle
  elDynAttr "span" (fst <$> cfg) $ dynText (snd <$> cfg)

  el "br" blank
  text "Episode slug (no special chars allowed): "
  el "br" blank
  dynText $ do mt <- title
               let str = (convertToFilename . toUpper . fromMaybe "") mt
               md <- date
               pure $ fromMaybe "" md <> "-" <> str

  el "br" blank
  sendButton <- button "send"

  let eitherEpisodeNew = do
        mCustomIndex <- customIndex
        mTitle       <- title
        mDate        <- date
        let mEpisodeNew = EpisodeNew <$> mCustomIndex <*> mTitle <*> mDate
        pure $ maybe (Left "all fields are required") Right mEpisodeNew
  -- with auth
  -- res <- postEpisodeNew (constDyn $ Left "no jwt") eitherEpisodeNew sendButton
  elAttr "p" (style $ color red) $ prerender_ (text "loading") $ do
    -- TODO: make client aware of currently selected show
    res <- lift $ postEpisodeNew (constDyn $ Right "fullserendipity")
                                 (constDyn $ Left "no jwt")
                                 eitherEpisodeNew
                                 sendButton
    let err = mapMaybe reqFailure res
    holdDyn "" err >>= dynText

  el "h1" $ text "Welcome to Obelisk!"
  el "p" $ text "foooooo"
  pure ()

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
