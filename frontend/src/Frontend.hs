{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Frontend where

import qualified Data.Map                  as Map
import qualified Data.Text                 as Text
import           Text.RawString.QQ         (r)

import           Obelisk.Frontend          (ObeliskWidget, Frontend (..))
import           Obelisk.Generated.Static  (static)
import           Obelisk.Route             (R)

import           Reflex.Dom.Core           (inputElementConfig_setValue, InputElement (..), blank, button,
                                            el, elAttr, text, (=:))

import           Client                    (postEpisodeNew)
import           Common                    (EpisodeNew (..), convertToFilename)
import           Control.Applicative       (Applicative (pure, (<*>)))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)))
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Default              (Default (def))
import           Data.Either               (Either (..))
import           Data.Function             (($), (&))
import           Data.Functor              (($>), Functor (fmap), (<$>))
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, toUpper)
import           Data.Time                 (defaultTimeLocale,
                                            formatTime, getCurrentTime)
import           Data.Tuple                (fst, snd)
import           Data.Witherable           (mapMaybe)
import           Reflex.Dom                (PerformEvent(performEvent), PostBuild(getPostBuild), DomBuilder (inputElement),
                                            MonadHold (holdDyn), constDyn, dynText,
                                            elDynAttr,
                                            elementConfig_initialAttributes,
                                            inputElementConfig_elementConfig,
                                            prerender_, (.~))
import           Route                     (FrontendRoute)
import           Servant.Common.Req        (reqFailure)
import Obelisk.Route.Frontend (RoutedT)

body
  :: forall t js (m :: * -> *)
  .  (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
body = do
    btnLogin <- el "div" $ button "Login"

    let input conf label id = do
          elAttr "label" ("for" =: id) $ text label
          el "br" blank
          i <- inputElement $ conf
                 & inputElementConfig_elementConfig
                 . elementConfig_initialAttributes .~ ("id" =: id)
          el "br" blank
          let str = _inputElement_value i
          pure $ fmap (\s -> if Text.null s then Nothing else Just s) str

    el "h1" $ text "Create new episode"
    pb <- getPostBuild
    postBuildTime <- performEvent $ pb $> liftIO getCurrentTime
    let today = Text.pack . formatTime defaultTimeLocale "%F" <$> postBuildTime
    date <- input (def & inputElementConfig_setValue .~ today
                  ) "Episode date: " "date"
    customIndex <- input def "Custom index: " "customIndex"
    title <- input def "Episode title: " "title"

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
          pure $ fromMaybe ("style" =: "font-style: italic", "empty") mTitle
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
    elAttr "p" ("style" =: "color:red") $ prerender_ (text "loading") $ do
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

style :: Text
style = [r|body {
  font-family: 'Abel', sans-serif;
  background-color: lightgray;
  margin: 0;
}|]

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      elAttr "meta" ( "content" =: "text/html;charset=utf-8"
                   <> "httpequiv" =: "content-type"
                    ) blank
      elAttr "meta" ( "content" =: "utf-8"
                   <> "httpequiv" =: "encoding"
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
      el "style" $ text style
  , _frontend_body = body
  }
