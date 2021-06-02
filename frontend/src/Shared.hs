{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Shared where

import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Category    (Category ((.)))
import           Control.Monad.Fix   (MonadFix)
import           Data.Bool           (Bool, not)
import           Data.Function       (const, ($))
import           Data.Functor        (Functor (fmap), void)
import           Data.Maybe          (Maybe (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unwords)
import qualified Data.Text           as Text
import           Data.Tuple          (fst)
import           Reflex.Dom          (DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventName (Click), EventResult,
                                      EventWriter (tellEvent),
                                      HasDomEvent (domEvent),
                                      InputElement (_inputElement_checked, _inputElement_value),
                                      InputElementConfig, MonadHold (holdDyn),
                                      Reflex (Dynamic, Event, current, updated),
                                      XhrResponse (..), attachWith, blank, def,
                                      el, el', elAttr, elAttr', elClass',
                                      elementConfig_initialAttributes, ffor,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialChecked,
                                      inputElementConfig_setChecked, leftmost,
                                      text, (&), (.~), (=:))
import           Servant.Common.Req  (ReqResult (..))
import           State               (EStateUpdate (..), State)

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

checkbox ::
  ( DomBuilder t m
  , MonadHold t m
  , MonadFix m
  ) => Bool -> Text -> m (Dynamic t Bool)
checkbox initial description = mdo
    cb <- inputElement $
      def & inputElementConfig_elementConfig
          . elementConfig_initialAttributes .~ "type" =: "checkbox"
          & inputElementConfig_initialChecked .~ initial
          & inputElementConfig_setChecked .~ eClickCB
    (elSpan, _) <- el' "span" $ text description
    let dynCbChecked = _inputElement_checked cb
        eToggle = leftmost [void $ updated dynCbChecked, domEvent Click elSpan]
        eClickCB = attachWith (const . not) (current dynCbChecked) eToggle
    holdDyn initial eClickCB

reqFailure :: ReqResult tag a -> Maybe Text
reqFailure = \case
  ResponseSuccess {}        -> Nothing
  ResponseFailure _ _   xhr -> _xhrResponse_responseText xhr
  RequestFailure  _ str     -> Just str

updateState ::
  ( Reflex t
  , EventWriter t EStateUpdate m
  ) => Event t (State -> State) -> m ()
updateState event =
  tellEvent $ fmap EStateUpdate event
