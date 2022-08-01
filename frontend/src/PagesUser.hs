{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module PagesUser
  ( pageRegister
  , pageLogin
  , pageAliasSelect
  , pageAliasRename
  ) where

import           Client                 (getAliasAll, getAuthData,
                                         postAliasRename, postAliasSetDefault,
                                         postAuthNew, postAuthenticate,
                                         postDoesUserExist, request)
import           Common.Auth            (LoginData (..), SessionData (..),
                                         UserNew (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Category       (Category (id, (.)))
import           Control.Lens.Setter    ((?~), set, (.~))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Bool              (Bool (..), bool, not)
import           Data.Either            (Either (..), either)
import           Data.Function          (($), (&))
import           Data.Functor           (Functor (fmap), void, ($>), (<$>))
import           Data.Generics.Product  (field)
import           Data.Maybe             (Maybe (Just, Nothing), maybe)
import           Data.Traversable       (forM)
import           Data.Tuple             (snd)
import           Data.Witherable        (Filterable (catMaybes, mapMaybe),
                                         filter)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute (..), routeLink)
import           Reflex.Dom             (fanEither, DomBuilder,
                                         EventName (Click),
                                         EventWriter,
                                         HasDomEvent (domEvent),
                                         InputElement (..), Key (Enter),
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild),
                                         Prerender,
                                         Reflex (Dynamic, never, updated), constDyn, def,
                                         dyn, dyn_, el, elAttr, elAttr',
                                         elementConfig_modifyAttributes, ffor,
                                         inputElementConfig_elementConfig,
                                         inputElementConfig_initialValue,
                                         keypress, leftmost, switchHold, text,
                                         widgetHold_, zipDyn, (=:))
import           Common.Route                  (FrontendRoute (..))
import           Servant.Common.Req     (reqSuccess)
import           Shared                 (btnSend, checkbox, elLabelInput,
                                         elLabelPasswordInput, iFa, updateState)
import           State                  (EStateUpdate (..), Session (..),
                                         State (..))
import Data.Functor ((<&>))

divOverlay
  :: forall t (m :: * -> *) a.
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender t m
  ) => m a -> m a
divOverlay inner =
  let cls = "class" =: "mkOverlay"
  in  elAttr "div" cls $ do
        let spanClose = elAttr "span" ("style" =: "float:right") $ iFa "fas fa-times"
        routeLink (FrontendRoute_Main :/ ()) spanClose
        inner

-- TODO: bug: on reload on this page, stack overflow
pageRegister
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender t m
  , MonadHold t m
  , MonadFix m
  , EventWriter t EStateUpdate m
  ) =>  m ()
pageRegister =
  divOverlay $ mdo
    let divFieldDescription = elAttr "div" $ "class" =: "onDesktopMaxWidth370px"
    (userName, iUserName) <- elLabelInput def "Username" "username"
    divFieldDescription $ text "Your user name is not publicly visible. You \
                               \can choose your public alias in the next step."
    let conf = def
          & inputElementConfig_elementConfig
          . elementConfig_modifyAttributes
          .~ (bool ("type" =: Just "text") ("type" =: Just "password") <$> eClickCb)
    (password, _) <- elLabelInput conf "Password" "password"
    eClickCb <- updated <$> checkbox False "Hide password input"
    divFieldDescription $ text "You enter your password only once. There are \
                               \no invalid passwords except for an empty one.\
                               \ Password reset via email can be optionally added later."
    eSend <- btnSend $ text "Send"
    let eFocusLost = void $ filter not $ updated $ _inputElement_hasFocus iUserName
        eUserName = maybe (Left "user empty") Right <$> userName
    (_, evUserExists) <- fanEither <$>
      request (postDoesUserExist eUserName eFocusLost)
    updateState $ filter id evUserExists $> (field @"stMsg" ?~ "username already exists")
    dynExists <- holdDyn False evUserExists
    let eUserNew = ffor (zipDyn dynExists $ zipDyn userName password) $ \case
          (True, _)             -> Left "username already exists"
          (_, (Just u, Just p)) -> Right $ UserNew u p Nothing True
          _                     -> Left "all fields are required"
    (evFailure, evSuccess) <- fanEither <$> request (postAuthNew eUserNew eSend)
    updateState $ set (field @"stSession") . SessionUser <$> evSuccess
    updateState $ set (field @"stMsg") . Just <$> evFailure
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ evSuccess $> FrontendRoute_AliasRename :/ ()

-- TODO: bug: 400 user not found is not caught
pageLogin
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender t m
  , MonadHold t m
  , EventWriter t EStateUpdate m
  ) =>  m ()
pageLogin =
  divOverlay $ do
    (userName, _) <- elLabelInput def "Username" "username"
    (password, inputPwd) <- elLabelPasswordInput def "Password" "password"
    eSend <- btnSend $ text "Login"
    let eEnter = keypress Enter inputPwd
        eLoginData = ffor (zipDyn userName password) $ \case
          (Just u, Just p) -> Right $ LoginData u p
          _                -> Left "all fields are required"
    (evFailure, evSuccess) <- fanEither <$> request (postAuthenticate eLoginData $ leftmost [eSend, eEnter])
    let evLoggedIn = catMaybes evSuccess
    updateState $ evSuccess <&> \case
      Just sd -> field @"stSession" .~ SessionUser sd
      Nothing -> field @"stMsg"     .~ Just "wrong password"
    updateState $ set (field @"stMsg") . Just <$> evFailure
    authData <- holdDyn (Left "not logged in yet") $ evLoggedIn <&>
      \SessionData{..} -> Right (sdJwt, sdAliasName)
    (_, evAliases) <- fanEither <$> request (getAliasAll authData $ void evLoggedIn)
    updateState $ set (field @"stMsg") . Just <$> evFailure
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ evAliases <&> \case
      [_] -> FrontendRoute_Main :/ ()
      _   -> FrontendRoute_AliasSelect :/ ()

pageAliasSelect
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Prerender t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , MonadReader (Dynamic t State) m
  ) => m ()
pageAliasSelect = do
  pb <- getPostBuild
  let loading = do
        iFa "fas fa-spinner fa-spin"
        text " Loading ..."
      css = "style" =: "aliasSelect"
  authData <- asks getAuthData
  divOverlay $ do
    (_, evAliases) <- fanEither <$> request (getAliasAll authData pb)
    widgetHold_ loading $ evAliases <&> \as -> mdo
      el "h2" $ text "Select alias"
      es <- forM as $ \alias -> do
        (elDiv, _) <- elAttr' "div" css $ text alias
        let eClick = domEvent Click elDiv
            dynAlias = constDyn $ Right alias
        eEDone <- dyn $ dynCbChecked <&> \checked ->
          if checked
            then snd . fanEither <$> request (postAliasSetDefault authData dynAlias eClick)
            else pure eClick
        switchHold never eEDone
      dynCbChecked <- checkbox True "Make default"
      setRoute $ leftmost es $> FrontendRoute_Main :/ ()

pageAliasRename
  :: forall t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , Prerender t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  , MonadReader (Dynamic t State) m
  ) => m ()
pageAliasRename = do
  authData <- asks getAuthData
  let dynEAlias = fmap (fmap snd) authData
  divOverlay $
    dyn_ $ ffor dynEAlias $ either text $ \alias -> do
      let conf = def
            & inputElementConfig_initialValue .~ alias
      (mNew, i) <- elLabelInput conf "Choose an alias" "newalias"
      eSend <- btnSend $ text "send"
      let eEnter = keypress Enter i
          eNew = maybe (Left "alias cannot be empty") Right <$> mNew
      (_, evSuccess) <- fanEither <$> request (postAliasRename authData eNew $ leftmost [eSend, eEnter])
      setRoute $ evSuccess $> FrontendRoute_Main :/ ()
