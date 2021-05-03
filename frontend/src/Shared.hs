{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}

module Shared where

import           Clay                (Color, Css, None (none), a, after,
                                      backgroundColor, body, bold, border,
                                      borderBox, borderRadius, both, boxShadow,
                                      boxSizing, bsColor, button, clear, color,
                                      content, display, displayTable, easeInOut,
                                      em, float, floatRight, focus, fontFamily,
                                      fontSize, fontWeight, gray, input,
                                      lightgray, margin, marginBottom,
                                      marginTop, outline, padding, pct, pt, px,
                                      renderWith, rgb, sansSerif, sec,
                                      shadowWithBlur, solid, star,
                                      stringContent, transition, visited, white,
                                      width, ( # ), (?), (^=))
import           Clay.Render         (htmlInline)
import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Category    (Category ((.)))
import           Control.Monad.Fix   (MonadFix)
import           Data.Bool           (Bool (..), not)
import           Data.Default        (Default (def))
import           Data.Foldable       (traverse_)
import           Data.Function       (const, ($), (&))
import           Data.Functor        (void)
import           Data.Map            (Map)
import           Data.Maybe          (Maybe (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy      as Lazy
import           Data.Tuple          (fst)
import           MediaQuery          (ResponsiveClass (rcClass, rcCss),
                                      desktopOnly, onDesktopBorder,
                                      onDesktopDisplayImportant,
                                      onDesktopDisplayNone,
                                      onDesktopMaxWidth370px,
                                      onDesktopMkOverlay, onMobileAtBottom,
                                      onMobileDisplayNone, onMobileFloatLeft,
                                      onMobileFloatRight, onMobileFontBig,
                                      onMobileHeight80, onMobileMkOverlay,
                                      onMobileWidthAuto, onMobileWidthFull,
                                      respClasses)
import           Reflex.Dom          (MonadHold(holdDyn), AttributeName,
                                      DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventName (Click), EventResult,
                                      HasDomEvent (domEvent), InputElement (..),
                                      InputElementConfig,
                                      Reflex (Dynamic, Event, current, updated),
                                      attachWith, blank, el, el', elAttr,
                                      elAttr', elClass',
                                      elementConfig_initialAttributes, ffor,
                                      inputElementConfig_elementConfig,
                                      inputElementConfig_initialChecked,
                                      inputElementConfig_setChecked, leftmost,
                                      text, (.~), (=:))

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

neonpink :: Color
neonpink = rgb 254 1 184

navBorder :: Css
navBorder = border solid (px 1) lightgray

style :: Css -> Map Text Text
style css = "style" =: Lazy.toStrict (renderWith htmlInline [] css)

styleA :: Css -> Map AttributeName Text
styleA css = "style" =: Lazy.toStrict (renderWith htmlInline [] css)

elClassStyle :: DomBuilder t m => Text -> Text -> Css -> m a -> m a
elClassStyle e class' css inner =
  let attrs = "class" =: class'
           <> style css
  in  elAttr e attrs inner

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
         .~ ("id" =: id <> "class" =: rcClass onMobileWidthFull)
  let dynStr = _inputElement_value i
      dynMStr = ffor dynStr $ \s -> if Text.null s then Nothing else Just s
  pure (dynMStr, i)

btnSend
  :: DomBuilder t m
  => m ()
  -> m (Event t ())
btnSend inner = do
  let cls = respClasses
        [ onDesktopMaxWidth370px
        , onMobileFontBig
        ]
      css = do width $ pct 100
               myBorderRadius
               padding (px 8) (px 8) (px 8) (px 8)
               backgroundColor neonpink
               color white
               fontWeight bold
               marginTop (em 1)
  (e, _) <- elAttr' "button" (cls <> style css) inner
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

myBorderRadius :: Css
myBorderRadius = borderRadius (px 12) (px 12) (px 12) (px 12)

cssGeneral :: Css
cssGeneral = pure ()
