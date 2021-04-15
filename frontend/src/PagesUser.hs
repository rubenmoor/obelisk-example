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
  , pageAliasSelect
  , pageAliasRename
  ) where

import           Clay                   (absolute, backgroundColor, border,
                                         color, em, float, floatRight,
                                         lightgrey, padding, position, px, red,
                                         solid, white, width, zIndex)
import           Client                 (getAliasAll, postAliasRename,
                                         postAliasSetDefault, postAuthNew,
                                         postAuthenticate, postDoesUserExist,
                                         request)
import           Common.Auth            (CompactJWT, LoginData (..),
                                         UserInfo (uiAlias), UserNew (..))
import           Control.Applicative    (Applicative (pure))
import           Control.Category       (Category (id, (.)))
import           Control.Monad          (Monad)
import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (Bool (..), not)
import           Data.Either            (Either (..), either)
import           Data.Foldable          (forM_, traverse_)
import           Data.Function          (const, ($), (&))
import           Data.Functor           (Functor (fmap), void, ($>), (<$>))
import           Data.List              (null)
import           Data.Maybe             (Maybe (Just, Nothing), maybe)
import           Data.Semigroup         (Semigroup ((<>)))
import           Data.Text              (Text)
import           Data.Traversable       (forM)
import           Data.Tuple             (fst, snd)
import           Data.Witherable        (Filterable (catMaybes, mapMaybe),
                                         filter)
import           MediaQuery             (onDesktopMaxWidth370px,
                                         onDesktopMkOverlay, onMobileMkOverlay,
                                         respClass, respClasses)
import           Model                  (Alias (aliasName))
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
                                         Reflex (Dynamic, current, never, updated),
                                         attachWith, blank, button, checkbox,
                                         constDyn, def, dyn, dyn_, el, el',
                                         elAttr, elAttr',
                                         elementConfig_initialAttributes, ffor,
                                         inputElementConfig_elementConfig,
                                         inputElementConfig_initialChecked,
                                         inputElementConfig_initialValue,
                                         inputElementConfig_setChecked,
                                         keypress, leftmost, prerender_,
                                         switchHold, text, widgetHold_, zipDyn,
                                         (.~), (=:))
import           Route                  (FrontendRoute (..))
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
    (password, _) <- elLabelInput def "Password" "password"
    widgetHold_ blank $ ffor success $ \case
      Just _ -> blank
      Nothing -> elAttr "span" (style $ color red) $ text "wrong password"
    eSend <- btnSend $ text "Login"
    let eLoginData = ffor (zipDyn userName password) $ \case
          (Just u, Just p) -> Right $ LoginData u p
          _                -> Left "all fields are required"
    response <- request (postAuthenticate eLoginData eSend)
    let success = mapMaybe reqSuccess response
        loggedIn = catMaybes success
    tellEvent $ ffor loggedIn $ \(jwt, ui) ->
      EStateUpdate (\s -> s { stSession = SessionUser jwt ui })
    dynEJwt <- holdDyn (Left "not logged in yet") $ Right . fst <$> loggedIn
    dynEAlias <- holdDyn (Left "not logged in yet") $
      Right . aliasName . uiAlias . snd <$> loggedIn
    respAliases <- mapMaybe reqSuccess <$>
      request (getAliasAll dynEJwt dynEAlias $ void loggedIn)
    -- TODO: store current route in route /register/ to allow going back
    setRoute $ ffor respAliases $ \ls ->
      if null ls then FrontendRoute_Main :/ ()
                 else FrontendRoute_AliasSelect :/ ()

getESession :: Session -> Either Text (CompactJWT, UserInfo)
getESession SessionAnon          = Left "not logged in"
getESession (SessionUser jwt ui) = Right (jwt, ui)

getEJwt :: Session -> Either Text CompactJWT
getEJwt = fmap fst . getESession

getEAlias  :: Session -> Either Text Text
getEAlias = fmap (aliasName . uiAlias . snd) . getESession

pageAliasSelect
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , Prerender js t m
  , Reflex t
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  ) => Dynamic t Session -> m ()
pageAliasSelect dynSession = do
  pb <- getPostBuild
  let dynEJwt = getEJwt <$> dynSession
      dynEAlias = getEAlias <$> dynSession
      loading = do
        iFa "fas fa-spinner fa-spin"
        text " Loading ..."
      css = style $ do
        border solid (px 1) lightgrey
        padding (px 8) (px 8) (px 8) (px 8)
        width (em 32)
  divOverlay $ do
    eAliases <- mapMaybe reqSuccess <$>
                request (getAliasAll dynEJwt dynEAlias pb)
    widgetHold_ loading $ ffor eAliases $ \as -> mdo
      el "h2" $ text "Select alias"
      es <- forM as $ \alias -> do
        (elDiv, _) <- elAttr' "div" css $ text alias
        let eClick = domEvent Click elDiv
            dynAlias = constDyn $ Right alias
        eEDone <- dyn $ ffor dynCbChecked $ \checked ->
          if checked
            then mapMaybe reqSuccess <$>
                   request (postAliasSetDefault dynEJwt dynEAlias dynAlias eClick)
            else pure eClick
        switchHold never eEDone
      (elCb, dynCbChecked) <- el' "div" $ do
        cb <- inputElement $
          def & inputElementConfig_elementConfig
              . elementConfig_initialAttributes .~ "type" =: "checkbox"
              & inputElementConfig_initialChecked .~ True
              & inputElementConfig_setChecked .~ eClickCB
        text "Make default alias"
        pure $ _inputElement_checked cb
      let eClickCB = attachWith (const . not) (current dynCbChecked) $ domEvent Click elCb
      setRoute $ leftmost es $> FrontendRoute_Main :/ ()

pageAliasRename
  :: forall js t (m :: * -> *).
  ( DomBuilder t m
  , PostBuild t m
  , Prerender js t m
  , RouteToUrl (R FrontendRoute) m
  , SetRoute t (R FrontendRoute) m
  ) => Dynamic t Session -> m ()
pageAliasRename dynSession = do
  let dynEJwt = getEJwt <$> dynSession
      dynEAlias = getEAlias <$> dynSession
  divOverlay $
    dyn_ $ ffor dynEAlias $ either text $ \alias -> do
      let conf = def
            & inputElementConfig_initialValue .~ alias
      (mNew, i) <- elLabelInput conf "Choose an alias" "newalias"
      eSend <- btnSend $ text "send"
      let eEnter = keypress Enter i
          eNew = maybe (Left "alias cannot be empty") Right <$> mNew
      resp <- request (postAliasRename dynEJwt dynEAlias eNew $ leftmost [eSend, eEnter])
      let success = mapMaybe reqSuccess resp
      setRoute $ success $> FrontendRoute_Main :/ ()
