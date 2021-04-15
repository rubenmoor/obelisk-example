{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Shared where

import           Clay                (visited, a, marginTop, em, button, white, bold, fontWeight, Color, Css, None (none), after,
                                      backgroundColor, body, border, borderBox,
                                      borderRadius, both, boxShadow, boxSizing,
                                      bsColor, clear, color, content, display,
                                      displayTable, easeInOut, float,
                                      floatRight, focus, fontFamily, fontSize,
                                      gray, input, lightgray, margin,
                                      marginBottom, outline, padding, pct, pt,
                                      px, renderWith, rgb, sansSerif, sec,
                                      shadowWithBlur, solid, star,
                                      stringContent, transition, width, ( # ),
                                      (?), (^=))
import           Clay.Render         (htmlInline)
import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Category    (Category ((.)))
import           Data.Foldable       (traverse_)
import           Data.Function       (($), (&))
import           Data.Functor        (void)
import           Data.Map            (Map)
import           Data.Maybe          (Maybe (..))
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy      as Lazy
import           Data.Tuple          (fst)
import           MediaQuery
import           Reflex.Dom          (ffor, elAttr', EventName(Click), HasDomEvent(domEvent), AttributeName,
                                      DomBuilder (DomBuilderSpace, inputElement),
                                      Element, EventResult, InputElement (..),
                                      InputElementConfig, Reflex(Event, Dynamic),
                                      blank, el, elAttr, elClass',
                                      elementConfig_initialAttributes,
                                      inputElementConfig_elementConfig, text,
                                      (.~), (=:))

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

myBorderRadius :: Css
myBorderRadius = borderRadius (px 12) (px 12) (px 12) (px 12)

cssGeneral :: Css
cssGeneral = do
  body ? do
    fontFamily ["Abel"] [sansSerif]
    -- fontSize (pt 42)
    fontSize (pt 18)
    color anthrazit
    backgroundColor lightgray
    margin (px 0) (px 0) (px 0) (px 0)
  star ? boxSizing borderBox
  star # ("class" ^= "col-") ? do
    float floatRight
    padding (px 5) (px 5) (px 5) (px 5)
    width (pct 100)
  desktopOnly $ do
    ".col-1"  ? width (pct 8.33)
    ".col-2"  ? width (pct 16.66)
    ".col-3"  ? width (pct 25)
    ".col-4"  ? width (pct 33.33)
    ".col-5"  ? width (pct 41.66)
    ".col-6"  ? width (pct 50)
    ".col-7"  ? width (pct 58.33)
    ".col-8"  ? width (pct 66.66)
    ".col-9"  ? width (pct 75)
    ".col-10" ? width (pct 83.33)
    ".col-11" ? width (pct 91.66)
    ".col-12" ? width (pct 100)
    body ? fontSize (pt 14)
  ".row" # after ? do
    content $ stringContent ""
    clear both
    display displayTable
  ".row" ? width (pct 100)

  a ? color anthrazit
  a # visited ? color anthrazit

  input ? do
    fontSize (pt 24)
    padding (px 8) (px 8) (px 8) (px 8)
    border solid (px 1) gray
    marginBottom (px 12)
    outline none (px 0) gray
    myBorderRadius
    transition "all" (sec 0.3) easeInOut (sec 0)
  input # focus ? do
    myBorderRadius
    boxShadow [bsColor neonpink $ shadowWithBlur (px 0) (px 0) (px 5)]

  button ? do
    fontSize (pt 18)
    fontFamily ["Abel"] [sansSerif]
    border none (px 0) white

  (traverse_ :: (ResponsiveClass -> Css) -> [ResponsiveClass] -> Css) rcCss
    [ onMobileDisplayNone
    , onDesktopDisplayNone
    , onMobileFloatLeft
    , onMobileFloatRight
    , onMobileAtBottom
    , onMobileHeight80
    , onDesktopDisplayImportant
    , onMobileWidthAuto
    , onDesktopBorder navBorder
    , onMobileFontBig
    , onMobileMkOverlay
    , onDesktopMkOverlay
    , onMobileWidthFull
    , onDesktopMaxWidth370px
    ]
