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

module Elements.Common where

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
import           Reflex.Dom          (dynText, elClass, foldDynMaybe, el, Prerender, PostBuild, DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventName (Click), EventResult,
                                      EventWriter,
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
import Data.Witherable (Filterable (catMaybes))

elOverlay
  :: forall t (m :: * -> *) a
  . ( DomBuilder t m
    , MonadFix m
    , MonadHold t m
    , PostBuild t m
    , Prerender t m
    , RouteToUrl (R FrontendRoute) m
    , SetRoute t (R FrontendRoute) m
    )
  => Event t Bool
  -> m a
  -> m (Event t a)
elOverlay evVisible inner = mdo
  evHide <- switchHold never $ fst <$> evDyn
  dynVisible <- foldDyn const False $ leftmost [evVisible, evHide $> False]
  evDyn <- dyn $ dynVisible <&> \case
    False -> pure (never, Nothing)
    True  -> elClass "div" "mkOverlay" $ do
      (domClose, _) <- elAttr' "span" ("style" =: "float:right") $ iFa "fas fa-times"
      v <- inner
      pure $ (domEvent Click domClose, Just v)
  pure $ catMaybes $ snd <$> evDyn

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
      dynMStr = dynStr <&> \s -> if Text.null s then Nothing else Just s
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
      dynMStr = dynStr <&> \s -> if Text.null s then Nothing else Just s
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
