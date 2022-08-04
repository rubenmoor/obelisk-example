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
{-# LANGUAGE BlockArguments #-}

module Elements.Auth
  ( elSignup
  , elLogin
  , elAliasSelect
  , elAliasRename
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
import           Data.Traversable       (for)
import           Data.Tuple             (snd)
import           Data.Witherable        (Filterable (catMaybes),
                                         filter)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute (..), routeLink)
import           Reflex.Dom             (fanEither, DomBuilder,
                                         Event,
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
                                         keypress, leftmost, switchDyn, switchHold, text,
                                         widgetHold, zipDyn, (=:))
import           Common.Route                  (FrontendRoute (..))
import           Elements.Common         (elLabelCheckbox, btnSend, elLabelInput,
                                         elLabelPasswordInput, iFa, elOverlay)
import           State                   (EStateUpdate (..), Session (..),
                                         State (..), updateState)
import Data.Foldable (length)
import Data.Ord ((>))
import Data.Functor ((<&>))

-- TODO: bug: on reload on this page, stack overflow
elSignup
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t EStateUpdate m
    , PostBuild t m
    , Prerender t m
    , MonadHold t m
    , MonadFix m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    )
  => Event t ()
  -> m (Event t ())
elSignup evShow = mdo
  evSuccess <- switchHold never evEvSuccess
  evEvSuccess <- elOverlay (leftmost [evShow $> True, evSuccess $> False]) mdo
    let divFieldDescription = elAttr "div" $ "class" =: "onDesktopMaxWidth370px"
    (userName, iUserName) <- elLabelInput def "Username" "username"
    divFieldDescription $ text "Your user name is not publicly visible. You \
                               \can choose your public alias in the next step."
    let conf = def
          & inputElementConfig_elementConfig
          . elementConfig_modifyAttributes
          .~ (bool ("type" =: Just "text") ("type" =: Just "password") <$> eClickCb)
    (password, _) <- elLabelInput conf "Password" "password"
    eClickCb <- updated <$> el "p" (elLabelCheckbox False "Hide password input" "hide-password")
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
    pure $ void evSuccess
  pure $ void evSuccess

-- TODO: bug: 400 user not found is not caught
elLogin
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , EventWriter t EStateUpdate m
    , MonadHold t m
    , MonadFix m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    )
  => Event t ()
  -> m (Event t ())
elLogin evShow = mdo
  evAliases <- switchHold never evEvAliases
  evEvAliases <- elOverlay (leftmost [evShow $> True, evAliases $> False]) do
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
    pure evAliases
  pure $ void $ filter (\ls -> length ls > 1) evAliases

elAliasSelect
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    , MonadReader (Dynamic t State) m
    )
  => Event t ()
  -> m (Event t ())
elAliasSelect evShow = mdo
  evSuccess <- switchHold never evEvSuccess
  evEvSuccess <- elOverlay (leftmost [evShow $> True, evSuccess $> False]) $ do

    let
        loading = do
          iFa "fas fa-spinner fa-spin"
          text " Loading ..."

        css = "style" =: "aliasSelect"

    authData <- asks getAuthData
    pb <- getPostBuild
    (_, evAliases) <- fanEither <$> request (getAliasAll authData pb)

    dynEvSuccess <- widgetHold (loading $> never) $ evAliases <&> \as -> mdo

      el "h2" $ text "Select alias"
      es <- for as $ \alias -> do

        (elDiv, _) <- elAttr' "div" css $ text alias
        let eClick = domEvent Click elDiv
            dynAlias = constDyn $ Right alias
        eEDone <- dyn $ dynCbChecked <&> \checked ->
          if checked
            then snd . fanEither <$> request (postAliasSetDefault authData dynAlias eClick)
            else pure eClick
        switchHold never eEDone

      dynCbChecked <- elLabelCheckbox True "Make default" "alias-make-default"
      pure $ void $ leftmost es

    pure $ switchDyn dynEvSuccess

  pure evSuccess

elAliasRename
  :: forall t (m :: * -> *)
  . ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , MonadReader (Dynamic t State) m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    )
  => Event t ()
  -> m (Event t ())
elAliasRename evShow = mdo
  evSuccess' <- switchHold never evEvSuccess'
  evEvSuccess' <- elOverlay (leftmost [evShow $> True, evSuccess' $> False]) do
    authData <- asks getAuthData
    let dynEAlias = fmap snd <$> authData

    evEvSuccess <- dyn $ dynEAlias <&> \case

      Left str -> do
        text str
        pure never

      Right alias -> do
        let conf = def
              & inputElementConfig_initialValue .~ alias
        (mNew, i) <- elLabelInput conf "Choose an alias" "newalias"
        eSend <- btnSend $ text "send"
        let eEnter = keypress Enter i
            eNew = maybe (Left "alias cannot be empty") Right <$> mNew
        (_, evSuccess) <-
          fanEither <$> request (postAliasRename authData eNew $ leftmost [eSend, eEnter])
        pure $ void evSuccess

    switchHold never evEvSuccess

  pure evSuccess'
