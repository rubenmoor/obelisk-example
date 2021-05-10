{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (right, inlineFlex, marginRight, smaller, fontSizeCustom, darkgray, block, minHeight, pointer, Cursor(cursor), textAlign, div, span, lightgrey, em, marginTop, bold, fontWeight, zIndex, Auto (auto), Center (center), Color, Css,
                                None (none), a, absolute, after, alignItems,
                                backgroundColor, body, border, borderBox,
                                borderRadius, both, bottom, boxShadow,
                                boxSizing, bsColor, button, clear, color,
                                content, display, displayTable, easeInOut,
                                fixed, flex, float, floatLeft, floatRight,
                                focus, fontFamily, fontSize, gray, height,
                                important, input, justifyContent, left,
                                lightgray, margin, marginBottom, maxWidth,
                                outline, padding, paddingBottom, pct, position,
                                pt, putCss, px, queryOnly, rgb, sansSerif, sec,
                                shadowWithBlur, solid, star, stringContent, top,
                                transform, transition, translate, visited,
                                white, width, ( # ), (?), (^=))
import qualified Clay.Media    as Media
import           Data.Function (($))
import           GHC.IO        (IO)
import           System.IO     (putStrLn)

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

neonpink :: Color
neonpink = rgb 254 1 184

myBorderRadius :: Css
myBorderRadius = borderRadius (px 12) (px 12) (px 12) (px 12)

desktopOnly :: Css -> Css
desktopOnly = queryOnly Media.screen [Media.minWidth $ px 768]

mobileOnly :: Css -> Css
mobileOnly = queryOnly Media.screen [Media.maxWidth $ px 767]

navBorder :: Css
navBorder = border solid (px 1) lightgray

main :: IO ()
main = putCss $ do
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

  ".mkOverlay" ? do
    backgroundColor white
    padding (px 24) (px 24) (px 24) (px 24)
    position absolute
    zIndex 1

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
    ".onDesktopDisplayNone" ? display none

    ".onDesktopDisplayImportant" ? do
      important $ display flex
      alignItems center
      justifyContent center
    ".onDesktopBorder" ? navBorder
    ".mkOverlay" ? do
      top $ pct 50
      left $ pct 50
      transform (translate (pct $ -50) $ pct $ -50)
      maxWidth $ px 418
    ".onDesktopMaxWidth370px" ? maxWidth (px 370)


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

-- mkResponsiveClass :: (Css -> Css) -> Css -> Selector -> ResponsiveClass
-- mkResponsiveClass mq css className = ResponsiveClass
--   { rcClass = Text.drop 1 $ Lazy.toStrict $ renderSelector className
--   , rcCss   = mq $ className ? css
--   }

  mobileOnly $ do
    ".onMobileFloatRight" ? float floatRight
    ".onMobileFloatLeft" ? float floatLeft
    ".onMobileDisplayNone" ? display none
    ".onMobileAtBottom" ? do
      position fixed
      bottom (pct 0)
    ".onMobileHeight80" ? height (px 80)
    ".onMobileWidthAuto" ? do
      width auto
      float none
    ".onMobileFontBig" ? fontSize (pt 40)
    ".mkOverlay" ? do
      width $ pct 100
      height $ pct 100
      top $ px 0
      important $ paddingBottom $ px 100
    ".onMobileWidthFull" ? width (pct 100)

  button # ".btnSend" ? do
    width $ pct 100
    myBorderRadius
    padding (px 8) (px 8) (px 8) (px 8)
    backgroundColor neonpink
    color white
    fontWeight bold
    marginTop (em 1)

  span # ".aliasSelect" ? do
    border solid (px 1) lightgrey
    padding (px 8) (px 8) (px 8) (px 8)
    maxWidth (em 32)

  div # ".navBar" ? do
    textAlign center
    backgroundColor white
    zIndex 2

  span # ".navButton" ? do
    backgroundColor white
    navBorder
    cursor pointer
    minHeight $ px 60
    zIndex 2

  ".displayNone" ? display none
  ".displayBlock" ? display block

  span # ".btnLogoutAlias" ? do
    color darkgray
    fontSizeCustom smaller
    marginRight $ em 0.5

  span # ".navBtnHome" ? do
    minHeight $ px 60
    display inlineFlex
    alignItems center
    justifyContent center

  span # ".navBars" ? do
    position absolute
    right (px 0)
    padding (px 0) (px 8) (px 0) (px 8)
