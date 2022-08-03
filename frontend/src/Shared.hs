{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE PatternSynonyms #-}

module Shared where

import           Data.Generics.Product  (field)
import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Category    (Category ((.)))
import           Control.Monad.Fix   (MonadFix)
import           Data.Bool           (bool, Bool, not)
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap), void, (<&>), ($>))
import           Data.Maybe          (Maybe (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unwords)
import qualified Data.Text           as Text
import           Data.Tuple          (fst)
import           Reflex.Dom          (dynText, elClass, foldDynMaybe, el, Prerender, PostBuild, DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventName (Click), EventResult,
                                      EventWriter (tellEvent),
                                      HasDomEvent (domEvent),
                                      InputElement (_inputElement_checked, _inputElement_value),
                                      InputElementConfig, MonadHold,
                                      Reflex (Dynamic, Event), blank, def, dyn,
                                      elAttr, elAttr', elClass', elDynClass',
                                      elementConfig_initialAttributes, ffor,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialChecked,
                                      leftmost,
                                      switchHold, never,
                                      text, (&), (.~), (=:))
import           State               (Session (..), EStateUpdate (..), State (..))
import Obelisk.Route.Frontend (SetRoute, R, RouteToUrl, pattern (:/), routeLink)
import Common.Route (FrontendRoute (..))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Bool (Bool (..))
import Common.Auth (SessionData(..))

-- elementConfig_initialAttributes, ffor,
-- inputElementConfig_elementConfig,
-- inputElementConfig_initialChecked,
-- inputElementConfig_setChecked, leftmost,
-- text, (.~), (=:))

-- anthrazit :: Color
-- anthrazit = rgb 8 20 48 -- #081430;

-- neonpink :: Color
-- neonpink = rgb 254 1 184
--
-- navBorder :: Css
-- navBorder = border solid (px 1) lightgray
--
-- style :: Css -> Map Text Text
-- style css = "style" =: Lazy.toStrict (renderWith htmlInline [] css)
--
-- styleA :: Css -> Map AttributeName Text
-- styleA css = "style" =: Lazy.toStrict (renderWith htmlInline [] css)
--
-- elClassStyle :: DomBuilder t m => Text -> Text -> Css -> m a -> m a
-- elClassStyle e class' css inner =
--   let attrs = "class" =: class'
--            <> style css
--   in  elAttr e attrs inner

iFa' :: DomBuilder t m => Text -> m (Element EventResult (DomBuilderSpace m) t)
iFa' class' = fst <$> elClass' "i" class' blank

iFa :: DomBuilder t m => Text -> m ()
iFa = void . iFa'

elLabelInput
  :: DomBuilder t m
  => InputElementConfig e t (DomBuilderSpace m)
  -> Text
  -> Text
  -> m (Dynamic t (Maybe Text), InputElement e (DomBuilderSpace m) t)
elLabelInput conf label id = do
  elAttr "label" ("for" =: id) $ el "h3" $ text label
  i <- inputElement $ conf
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("id" =: id <> "type" =: "text")
  let dynStr = _inputElement_value i
      dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
  pure (dynMStr, i)

elLabelPasswordInput
  :: DomBuilder t m
  => InputElementConfig e t (DomBuilderSpace m)
  -> Text
  -> Text
  -> m (Dynamic t (Maybe Text), InputElement e (DomBuilderSpace m) t)
elLabelPasswordInput conf label id = do
  elAttr "label" ("for" =: id) $ el "h3" $ text label
  i <- inputElement $ conf
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("id" =: id <> "type" =: "password")
  let dynStr = _inputElement_value i
      dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
  pure (dynMStr, i)

btnSend
  :: DomBuilder t m
  => m ()
  -> m (Event t ())
btnSend inner = do
  let cls = "class" =: unwords
        [ "onDesktopMaxWidth370px"
        , "onMobileFontBig"
        , "btnSend"
        ]
  (e, _) <- elAttr' "button" cls inner
  pure $ domEvent Click e

elLabelCheckbox
    :: (DomBuilder t m) => Bool -> Text -> Text -> m (Dynamic t Bool)
elLabelCheckbox initial label strId = do
    cb <-
        inputElement
        $  def
        &  inputElementConfig_elementConfig
        .  elementConfig_initialAttributes
        .~ ("type" =: "checkbox" <> "id" =: strId)
        &  inputElementConfig_initialChecked
        .~ initial
    elAttr "label" ("for" =: strId) $ text $ " " <> label
    pure $ _inputElement_checked cb

updateState ::
  ( Reflex t
  , EventWriter t EStateUpdate m
  ) => Event t (State -> State) -> m ()
updateState event =
  tellEvent $ fmap EStateUpdate event

elNavigation
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
elNavigation = do
  let divNavBar = elAttr "div" ("class" =: unwords
        [ "row"
        , "onMobileAtBottom"
        , "onMobileFontBig"
        , "navBar"
        ])
  dynSession <- asks $ fmap stSession
  divNavBar $ mdo
    let navButtonCls = displayNav <&> \cls -> unwords
          [ "col-1"
          , "onMobileHeight80"
          , "onMobileBorder"
          , "onDesktopDisplayImportant"
          , "navButton"
          , cls
          ]
        spanNavBtn = fmap fst . elDynClass' "span" navButtonCls
    eBtns <- dyn $ dynSession <&> \case
      SessionAnon -> do
        elLogin <- routeLink (FrontendRoute_Login :/ ()) $
          spanNavBtn $ text "Login"
        elRegister <- routeLink (FrontendRoute_Register :/ ()) $
          spanNavBtn $ text "Sign up"
        pure $ leftmost $ domEvent Click <$> [elLogin, elRegister]
      SessionUser SessionData{..} -> do
        elSettings <- routeLink (FrontendRoute_Settings :/ ()) $
          spanNavBtn $ text "Settings"
        elLogout <- spanNavBtn $ do
          elAttr "span" ("class" =: "btnLogoutAlias") $ text sdAliasName
          text "Logout"
        let eLogout = domEvent Click elLogout
        updateState $ eLogout $> (field @"stSession" .~ SessionAnon)
        pure $ leftmost [eLogout, domEvent Click elSettings]
    eExit <- switchHold never eBtns

    let spanNavBtnHome = fmap fst . elAttr' "span"
          ("class" =: unwords [ "col-1"
                              , "onMobileWidthAuto"
                              , "navBtnHome"
                              ])
    elHome <- routeLink (FrontendRoute_Main :/ ()) $
      spanNavBtnHome $ do
        iFa "fas fa-home"
        elAttr "span" ("class" =: "onMobileDisplayNone"
                    <> "style" =: "margin-left:0.5em") $ text "Home"
        blank

    let spanNavBars =
          elAttr "span" ("class" =: unwords
            [ "onDesktopDisplayNone"
            , "navBars"
            ])
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
    let displayNav = bool "displayNone" "displayBlock" <$> dynToggle
    blank

elTitle
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
  ) => Dynamic t Text -> m ()
elTitle dynSubtitle = elClass "section" "title" do
    elClass "h1" "page-title" $ text "serendipity.works"
    el "h2" $ dynText dynSubtitle
    elClass "div" "gradient" blank
    elClass "div" "spacer-white onDesktopDisplayNone" blank
    elNavigation
