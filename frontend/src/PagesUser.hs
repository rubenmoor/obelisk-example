{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module PagesUser
  ( pageRegister
  ) where

import           Clay                (pct, width, None (none), absolute, backgroundColor,
                                      block, display, float, floatRight,
                                      padding, position, px, white, zIndex)
import           Control.Applicative (Applicative (pure))
import           Data.Function       (($))
import           Data.Semigroup      (Semigroup ((<>)))
import           MediaQuery          (onDesktopMaxWidth370px,
                                      onDesktopMkOverlay, onMobileMkOverlay,
                                      respClass, respClasses)
import           Reflex.Dom          (Prerender, PostBuild, DomBuilder, EventName (Click),
                                      HasDomEvent (domEvent),
                                      MonadHold (holdDyn), blank, button, def,
                                      elAttr, elAttr', elDynAttr, ffor,
                                      leftmost, text)
import           Shared              (btnSend, elLabelInput, iFa, style)
import Data.Tuple (fst)
import Data.Functor (($>), (<$>))
import Data.Bool (bool, Bool(False))
import Control.Monad.Fix (MonadFix)
import Route (FrontendRoute(FrontendRoute_Main))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Obelisk.Route (pattern (:/))
import Obelisk.Route (R)

pageRegister
  :: forall t js s (m :: * -> *).
  ( DomBuilder t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  ) =>  m ()
pageRegister = mdo
  let overlayAttrs =
        respClasses [ onMobileMkOverlay
                    , onDesktopMkOverlay
                    ] <>
        style (do backgroundColor white
                  padding (px 24) (px 24) (px 24) (px 24)
                  position absolute
                  zIndex 1
                  )
      divOverlay = elAttr "div" overlayAttrs
  (userName, password) <- divOverlay $ do
    let spanClose = elAttr' "span" (style $ float floatRight) $ iFa "fas fa-times"
        divFieldDescription = elAttr "div" $ respClass onDesktopMaxWidth370px
    routeLink (FrontendRoute_Main :/ ()) spanClose
    userName <- elLabelInput def "Username" "username"
    divFieldDescription $ text "Your user name is not publicly visible. You \
                               \can choose your public alias in the next step."
    password <- elLabelInput def "Password" "password"
    divFieldDescription $ text "You enter your password only once. There are \
                               \no invalid passwords except for an empty one.\
                               \ Password reset via email can be optionally added later."
    eSend <- btnSend $ text "Send"
    pure (userName, password)
  -- TODO: store current route in route /register/ to allow going back
  blank
