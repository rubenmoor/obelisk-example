{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MediaQuery where

import           Clay           (Center(center), alignItems, justifyContent, flex, inlineFlex, lightgray, solid, border, Auto (auto), Css, None (none), Selector,
                                 absolute, block, bottom, display, fixed, float,
                                 floatLeft, floatRight, fontSize, height,
                                 important, left, maxWidth, paddingBottom, pct,
                                 position, pt, px, queryOnly, renderSelector,
                                 top, transform, translate, width, (?))
import           Clay.Media     (screen)
import qualified Clay.Media     as Media
import           Data.Map       (Map)
import           Data.Text      (Text, unwords)
import qualified Data.Text      as Text
import qualified Data.Text.Lazy as Lazy
import Reflex.Dom ((=:))
import Control.Category ( Category((.)) )
import Data.Functor (Functor(fmap))
import Data.Function (($))

desktopOnly :: Css -> Css
desktopOnly = queryOnly screen [Media.minWidth $ px 768]

mobileOnly :: Css -> Css
mobileOnly = queryOnly screen [Media.maxWidth $ px 767]

data ResponsiveClass = ResponsiveClass
  { rcClass :: Text
  , rcCss   :: Css
  }

mkResponsiveClass :: (Css -> Css) -> Css -> Selector -> ResponsiveClass
mkResponsiveClass mq css className = ResponsiveClass
  { rcClass = Text.drop 1 $ Lazy.toStrict $ renderSelector className
  , rcCss   = mq $ className ? css
  }

onMobileFloatRight :: ResponsiveClass
onMobileFloatRight =
  mkResponsiveClass mobileOnly (float floatRight) ".onMobileFloatRight"

onMobileFloatLeft :: ResponsiveClass
onMobileFloatLeft =
  mkResponsiveClass mobileOnly (float floatLeft) ".onMobileFloatLeft"

onDesktopDisplayNone :: ResponsiveClass
onDesktopDisplayNone =
  mkResponsiveClass desktopOnly (display none) ".onDesktopDisplayNone"

onMobileDisplayNone :: ResponsiveClass
onMobileDisplayNone =
  mkResponsiveClass mobileOnly (display none) ".onMobileDisplayNone"

onDesktopDisplayImportant :: ResponsiveClass
onDesktopDisplayImportant =
  mkResponsiveClass desktopOnly
    (do important $ display flex
        alignItems center
        justifyContent center
    ) ".onDesktopDisplayImportant"

onMobileAtBottom :: ResponsiveClass
onMobileAtBottom =
  let css = do
        position fixed
        bottom (pct 0)
  in  mkResponsiveClass mobileOnly css ".onMobileAtBottom"

onMobileHeight80 :: ResponsiveClass
onMobileHeight80 =
  mkResponsiveClass mobileOnly (height $ px 80) ".onMobileHeight80"

onMobileWidthAuto :: ResponsiveClass
onMobileWidthAuto =
  mkResponsiveClass mobileOnly (do width auto
                                   float none
                               ) ".onMobileWidthAuto"

onDesktopBorder :: Css -> ResponsiveClass
onDesktopBorder css =
  mkResponsiveClass desktopOnly css ".onDesktopBorder"

onMobileFontBig :: ResponsiveClass
onMobileFontBig =
  mkResponsiveClass mobileOnly (fontSize $ pt 40) ".onMobileFontBig"

onMobileMkOverlay :: ResponsiveClass
onMobileMkOverlay =
  mkResponsiveClass mobileOnly
    (do width $ pct 100
        height $ pct 100
        top $ px 0
        important $ paddingBottom $ px 100
        ) ".onMobileMkOverlay"

onDesktopMkOverlay :: ResponsiveClass
onDesktopMkOverlay =
  mkResponsiveClass desktopOnly
    (do position absolute
        top $ pct 50
        left $ pct 50
        transform (translate (pct $ -50) $ pct $ -50)
        maxWidth $ px 418
    ) ".onDesktopMkOverlay"

onMobileWidthFull :: ResponsiveClass
onMobileWidthFull =
  mkResponsiveClass mobileOnly (width $ pct 100) ".onMobileWidthFull"

onDesktopMaxWidth370px :: ResponsiveClass
onDesktopMaxWidth370px =
  mkResponsiveClass desktopOnly (maxWidth $ px 370) ".onDesktopMaxWidth370px"

classes :: [Text] -> Map Text Text
classes ls = "class" =: unwords ls

respClasses :: [ResponsiveClass] -> Map Text Text
respClasses = classes . fmap rcClass

respClass :: ResponsiveClass -> Map Text Text
respClass rc = "class" =: rcClass rc
