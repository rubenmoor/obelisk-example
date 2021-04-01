{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module PagesUser
  ( pageRegister
  ) where

import           Clay                (red, color, None(none), inline, absolute, backgroundColor, float, floatRight,
                                      padding, position, px, white, zIndex)
import           Data.Function       (($))
import           Data.Semigroup      (Semigroup ((<>)))
import           MediaQuery          (onDesktopMaxWidth370px,
                                      onDesktopMkOverlay, onMobileMkOverlay,
                                      respClass, respClasses)
import           Reflex.Dom          (zipDyn, PostBuild, elDynAttr, ffor, switchDyn, MonadHold(holdDyn), Reflex(never, updated), Prerender(prerender),  DomBuilder, blank, def,
                                      elAttr, text, InputElement (..))
import           Shared              (btnSend, elLabelInput, iFa, style)
import Route (FrontendRoute(FrontendRoute_Main))
import Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)
import Obelisk.Route (pattern (:/))
import Obelisk.Route (R)
import Data.Witherable (Filterable(mapMaybe), filter)
import Data.Bool ((&&), Bool(..), not)
import Client (postAuthNew, postDoesUserExist)
import Data.Maybe (Maybe(Just), isJust, maybe)
import Data.Either (Either(..))
import Data.Functor (void, Functor(fmap), (<$>))
import Control.Applicative ((<$), Applicative(pure))
import Servant.Common.Req (reqSuccess)
import Clay (display)
import Control.Monad.Fix (MonadFix)
import Common.Auth (UserNew(..))

divOverlay
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  ) => m () -> m ()
divOverlay inner =
  let cls = respClasses
        [ onMobileMkOverlay
        , onDesktopMkOverlay
        ]
      css = do backgroundColor white
               padding (px 24) (px 24) (px 24) (px 24)
               position absolute
               zIndex 1
  in  elAttr "div" (cls <> style css) $ do
        let spanClose = elAttr "span" (style $ float floatRight) $ iFa "fas fa-times"
        routeLink (FrontendRoute_Main :/ ()) spanClose
        inner

pageRegister
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m
  ) =>  m ()
pageRegister =
  divOverlay $ mdo
    let divFieldDescription = elAttr "div" $ respClass onDesktopMaxWidth370px
    (userName, iUserName) <- elLabelInput def "Username" "username"
    divFieldDescription $ text "Your user name is not publicly visible. You \
                               \can choose your public alias in the next step."
    (password, _) <- elLabelInput def "Password" "password"
    divFieldDescription $ text "You enter your password only once. There are \
                               \no invalid passwords except for an empty one.\
                               \ Password reset via email can be optionally added later."
    elDynAttr "span" userExistsAttrs $ text "username already exists"
    eSend <- btnSend $ text "Send"
    let eFocusLost = void $ filter not $ updated $ _inputElement_hasFocus iUserName
        eUserName = maybe (Left "user empty") Right <$> userName
    respUExists <- switchDyn <$> prerender (pure never) (postDoesUserExist eUserName eFocusLost)
    dynExists <- holdDyn False $ mapMaybe reqSuccess respUExists
    let userExistsAttrs = ffor dynExists $ \e -> style $ do
          display $ if e then inline else none
          color red
        eUserNew = ffor (zipDyn dynExists $ zipDyn userName password) $ \case
          (True, _)             -> Left "username already exists"
          (_, (Just u, Just p)) -> Right $ UserNew u p
          _                     -> Left "all fields are required"
    respUserNew <- switchDyn <$> prerender (pure never) (postAuthNew eUserNew eSend)
    -- TODO: handle reqFailure
    -- TODO: store jwt
    blank
    -- TODO: store current route in route /register/ to allow going back
