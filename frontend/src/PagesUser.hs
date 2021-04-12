{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PagesUser
  ( pageRegister
  , pageLogin
  ) where

import           Clay                   (absolute, backgroundColor, color,
                                         float, floatRight, padding, position,
                                         px, red, white, zIndex)
import           Client                 (postAuthenticate, postAuthNew,
                                         postDoesUserExist, request)
import           Common.Auth            (LoginData (..), UserNew (..))
import           Control.Category       (Category (id))
import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (Bool (..), not)
import           Data.Either            (Either (..))
import           Data.Function          (($))
import           Data.Functor           (void, ($>), (<$>))
import           Data.Maybe             (Maybe (Just), maybe)
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.Witherable        (Filterable (mapMaybe),
                                         filter)
import           MediaQuery             (onDesktopMaxWidth370px,
                                         onDesktopMkOverlay, onMobileMkOverlay,
                                         respClass, respClasses)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute (..), routeLink)
import           Reflex.Dom             (DomBuilder, EventWriter (tellEvent),
                                         InputElement (..), MonadHold (holdDyn),
                                         PostBuild, Prerender (prerender),
                                         Reflex (updated), blank, def, elAttr,
                                         ffor, text, widgetHold_,
                                         zipDyn)
import           Route                  (FrontendRoute (FrontendRoute_Main))
import           Servant.Common.Req     (reqFailure, reqSuccess)
import           Shared                 (btnSend, elLabelInput, iFa, style)
import           State                  (EStateUpdate (..), Session (..),
                                         State (..))

divOverlay
  :: forall t js (m :: * -> *) a.
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  ) => m a -> m a
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
  , EventWriter t EStateUpdate m
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
    let widgetUserExists =
          widgetHold_ blank $ ffor (filter id eUserExists) $ \_ ->
            elAttr "span" (style $ color red) $ text "username already exists"
    widgetHold_ widgetUserExists $ ffor (mapMaybe reqFailure response) $ \strErr ->
      elAttr "span" (style $ color red) $ text strErr
    eSend <- btnSend $ text "Send"
    let eFocusLost = void $ filter not $ updated $ _inputElement_hasFocus iUserName
        eUserName = maybe (Left "user empty") Right <$> userName
    eUserExists <- mapMaybe reqSuccess <$>
      request (postDoesUserExist eUserName eFocusLost)
    dynExists <- holdDyn False eUserExists
    let eUserNew = ffor (zipDyn dynExists $ zipDyn userName password) $ \case
          (True, _)             -> Left "username already exists"
          (_, (Just u, Just p)) -> Right $ UserNew u p
          _                     -> Left "all fields are required"
    response <- request $ postAuthNew eUserNew eSend
    let success = mapMaybe reqSuccess response
    tellEvent $ ffor success $ \(jwt, ui) ->
      EStateUpdate (\s -> s { stSession = SessionUser jwt ui })
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ success $> FrontendRoute_Main :/ ()

pageLogin
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , PostBuild t m
  , EventWriter t EStateUpdate m
  ) =>  m ()
pageLogin =
  divOverlay $ mdo
    (userName, _) <- elLabelInput def "Username" "username"
    (password, _) <- elLabelInput def "Password" "password"
    eSend <- btnSend $ text "Login"
    let eLoginData = ffor (zipDyn userName password) $ \case
          (Just u, Just p) -> Right $ LoginData u p
          _                -> Left "all fields are required"
    eSession <- mapMaybe reqSuccess <$>
      request (postAuthenticate eLoginData eSend)
    tellEvent $ ffor eSession $ \session -> EStateUpdate (\s -> s { stSession = SessionAnon })
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ eSession $> FrontendRoute_Main :/ ()
