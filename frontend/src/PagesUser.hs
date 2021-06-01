{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
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
  , pageAliasSelect
  , pageAliasRename
  ) where

import           Client                 (getAuthData, getAliasAll, postAliasRename,
                                         postAliasSetDefault, postAuthNew,
                                         postAuthenticate, postDoesUserExist,
                                         request)
import           Common.Auth            (LoginData (..), SessionData (..),
                                         UserNew (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Category       (Category (id, (.)))
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.Reader   (asks, MonadReader)
import           Data.Bool              (Bool (..), bool, not)
import           Data.Either            (Either (..), either)
import           Data.Function          (($), (&))
import           Data.Functor           (Functor(fmap), void, ($>), (<$>))
import           Data.Maybe             (Maybe (Just, Nothing), maybe)
import           Data.Traversable       (forM)
import           Data.Witherable        (Filterable (catMaybes, mapMaybe),
                                         filter)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute (..), routeLink)
import           Reflex.Dom             (DomBuilder (inputElement),
                                         EventName (Click),
                                         EventWriter (tellEvent),
                                         HasDomEvent (domEvent),
                                         InputElement (..), Key (Enter),
                                         MonadHold (holdDyn),
                                         PostBuild (getPostBuild),
                                         Prerender (prerender),
                                         Reflex (Dynamic, never, updated),
                                         blank, constDyn, def, dyn, dyn_, el,
                                         elAttr, elAttr',
                                         elementConfig_modifyAttributes, ffor,
                                         inputElementConfig_elementConfig,
                                         inputElementConfig_initialValue,
                                         keypress, leftmost, switchHold, text,
                                         widgetHold_, zipDyn, (.~), (=:))
import           Route                  (FrontendRoute (..))
import           Servant.Common.Req     (reqFailure, reqSuccess)
import           Shared                 (btnSend, checkbox, elLabelInput, iFa)
import           State                  (EStateUpdate (..), Session (..),
                                         State (..))
import Data.Tuple (snd)

divOverlay
  :: forall t js (m :: * -> *) a.
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  ) => m a -> m a
divOverlay inner =
  let cls = "class" =: "mkOverlay"
  in  elAttr "div" cls $ do
        let spanClose = elAttr "span" ("style" =: "float:right") $ iFa "fas fa-times"
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
    let widgetUserExists =
          widgetHold_ blank $ ffor (filter id eUserExists) $ \_ ->
            elAttr "span" ("style" =: "color:red") $ text "username already exists"
    widgetHold_ widgetUserExists $ ffor (mapMaybe reqFailure response) $ \strErr ->
      elAttr "span" ("style" =: "color:red") $ text strErr
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
    tellEvent $ ffor success $ \sd ->
      EStateUpdate (\s -> s { stSession = SessionUser sd })
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ success $> FrontendRoute_AliasRename :/ ()

pageLogin
  :: forall t js (m :: * -> *).
  ( DomBuilder t m
  , SetRoute t (R FrontendRoute) m
  , RouteToUrl (R FrontendRoute) m
  , Prerender js t m
  , MonadHold t m
  , MonadFix m
  , EventWriter t EStateUpdate m
  ) =>  m ()
pageLogin =
  divOverlay $ mdo
    (userName, _) <- elLabelInput def "Username" "username"
    (password, inputPwd) <- elLabelInput def "Password" "password"
    widgetHold_ blank $ ffor success $ \case
      Just _ -> blank
      Nothing -> elAttr "span" ("style" =: "color:red") $ text "wrong password"
    eSend <- btnSend $ text "Login"
    let eEnter = keypress Enter inputPwd
        eLoginData = ffor (zipDyn userName password) $ \case
          (Just u, Just p) -> Right $ LoginData u p
          _                -> Left "all fields are required"
    response <- request $ postAuthenticate eLoginData $ leftmost [eSend, eEnter]
    let success = mapMaybe reqSuccess response
        loggedIn = catMaybes success
    tellEvent $ ffor loggedIn $ \sd ->
      EStateUpdate (\s -> s { stSession = SessionUser sd })
    authData <- holdDyn (Left "not logged in yet") $ ffor loggedIn $ \SessionData{..} ->
      Right (sdJwt, sdAliasName)
    respAliases <- mapMaybe reqSuccess <$>
      request (getAliasAll authData $ void loggedIn)
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ ffor respAliases $ \case
      [_] -> FrontendRoute_Main :/ ()
      _   -> FrontendRoute_AliasSelect :/ ()

pageAliasSelect
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Prerender js t m
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
    eAliases <- mapMaybe reqSuccess <$>
                request (getAliasAll authData pb)
    widgetHold_ loading $ ffor eAliases $ \as -> mdo
      el "h2" $ text "Select alias"
      es <- forM as $ \alias -> do
        (elDiv, _) <- elAttr' "div" css $ text alias
        let eClick = domEvent Click elDiv
            dynAlias = constDyn $ Right alias
        eEDone <- dyn $ ffor dynCbChecked $ \checked ->
          if checked
            then mapMaybe reqSuccess <$>
                   request (postAliasSetDefault authData dynAlias eClick)
            else pure eClick
        switchHold never eEDone
      dynCbChecked <- checkbox True "Make default"
      setRoute $ leftmost es $> FrontendRoute_Main :/ ()

pageAliasRename
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , Prerender js t m
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
      resp <- request (postAliasRename authData eNew $ leftmost [eSend, eEnter])
      let success = mapMaybe reqSuccess resp
      setRoute $ success $> FrontendRoute_Main :/ ()
