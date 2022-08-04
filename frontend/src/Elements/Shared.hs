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

module Elements.Shared where

import qualified Data.Map.Strict as Map
import           Data.Generics.Product  (field)
import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Category    (Category ((.)))
import           Control.Monad.Fix   (MonadFix)
import           Data.Bool           (bool, Bool, not)
import           Data.Function       (($), const)
import           Data.Functor        (Functor (fmap), void, (<&>), ($>))
import           Data.Maybe          (Maybe (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unwords)
import qualified Data.Text           as Text
import           Data.Tuple          (fst, snd)
import Data.Functor.Misc (Const2 (Const2))
import           Reflex.Dom          (dynText, elClass, foldDynMaybe, el, Prerender, PostBuild, DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventName (Click), EventResult,
                                      EventWriter,
                                      EventSelector (select),
                                      fanMap,
                                      HasDomEvent (domEvent),
                                      InputElement (_inputElement_checked, _inputElement_value),
                                      InputElementConfig, MonadHold,
                                      Reflex (Dynamic, Event), blank, def, dyn,
                                      elAttr, elAttr', elClass', elDynClass',
                                      elementConfig_initialAttributes, foldDyn,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialChecked,
                                      leftmost,
                                      switchHold, never,
                                      text, (&), (.~), (=:))
import           State               (Session (..), EStateUpdate (..), State (..), updateState)
import Obelisk.Route.Frontend (SetRoute, R, RouteToUrl, pattern (:/), routeLink)
import Common.Route (FrontendRoute (..))
import Control.Monad.Reader.Class (MonadReader, asks)
import Data.Bool (Bool (..))
import Common.Auth (SessionData(..))
import Elements.Auth (elLogin, elSignup)
import Elements.Common (iFa, iFa')
import Data.Eq (Eq)
import Data.Ord (Ord)

data NavEventTag
  = NavEventLogin
  | NavEventSignup
  | NavEventNothing
  deriving (Eq, Ord)

-- title element

elTitle
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
    )
  => Dynamic t Text
  -> m (Event t NavEventTag)
elTitle dynSubtitle = elClass "section" "title" do
    elClass "h1" "page-title" $ text "serendipity.works"
    el "h2" $ dynText dynSubtitle
    elClass "div" "gradient" blank
    elClass "div" "spacer-white onDesktopDisplayNone" blank
    elNavigation

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
  ) => m (Event t NavEventTag)
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
        elNavBtn = fmap fst . elDynClass' "span" navButtonCls

    evDyn <- dyn $ dynSession <&> \case

      SessionAnon -> do

        domLogin  <- elNavBtn $ text "Login"
        domSignup <- elNavBtn $ text "Sign up"

        let
            evLogin = domEvent Click domLogin
            evSignup = domEvent Click domSignup

        pure $ leftmost
          [ evLogin  $> NavEventLogin
          , evSignup $> NavEventSignup
          ]

      SessionUser SessionData{..} -> do

        domSettings <- routeLink (FrontendRoute_Settings :/ ()) $ elNavBtn $ text "Settings"
        domLogout <- elNavBtn $ do
          elAttr "span" ("class" =: "btnLogoutAlias") $ text sdAliasName
          text "Logout"

        let evLogout = domEvent Click domLogout

        updateState $ evLogout $> (field @"stSession" .~ SessionAnon)
        pure $ leftmost [evLogout, domEvent Click domSettings] $> NavEventNothing

    evNav <- switchHold never evDyn

    let elNavBtnHome = fmap fst . elAttr' "span"
          ("class" =: unwords [ "col-1"
                              , "onMobileWidthAuto"
                              , "navBtnHome"
                              ])
    elHome <- elNavBtnHome $ do
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
               , evNav   $> False
               , eHome   $> False
               ]
    let displayNav = bool "displayNone" "displayBlock" <$> dynToggle
    pure evNav

elContent
  :: forall a t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t EStateUpdate m
    , MonadFix m
    , MonadHold t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , MonadReader (Dynamic t State) m
    )
  => Event t NavEventTag
  -> m a
  -> m a
elContent evNav inner =
  elClass "section" "content" do

    let selector = fanMap $ evNav <&> \t -> Map.singleton t ()

    elLogin  $ select selector (Const2 NavEventLogin)
    elSignup $ select selector (Const2 NavEventSignup)

    inner

elPage
  :: forall a t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t EStateUpdate m
    , MonadFix m
    , MonadHold t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , MonadReader (Dynamic t State) m
    )
  => Dynamic t Text -> m a -> m a
elPage dynText inner = do
  evNav <- elTitle dynText
  elContent evNav inner
