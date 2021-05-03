{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Clay          (Color, Css, None (none), a, after,
                                backgroundColor, body, border, borderBox,
                                borderRadius, both, boxShadow, boxSizing,
                                bsColor, button, clear, color, content, display,
                                displayTable, easeInOut, float, floatRight,
                                focus, fontFamily, fontSize, gray, input,
                                lightgray, margin, marginBottom, outline,
                                padding, pct, pt, putCss, px, queryOnly, rgb,
                                sansSerif, sec, shadowWithBlur, solid, star,
                                stringContent, transition, visited, white,
                                width, (#), (?), (^=))
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
  queryOnly Media.screen [Media.minWidth $ px 768] $ do
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

  -- (traverse_ :: (ResponsiveClass -> Css) -> [ResponsiveClass] -> Css) rcCss
  --   [ onMobileDisplayNone
  --   , onDesktopDisplayNone
  --   , onMobileFloatLeft
  --   , onMobileFloatRight
  --   , onMobileAtBottom
  --   , onMobileHeight80
  --   , onDesktopDisplayImportant
  --   , onMobileWidthAuto
  --   , onDesktopBorder navBorder
  --   , onMobileFontBig
  --   , onMobileMkOverlay
  --   , onDesktopMkOverlay
  --   , onMobileWidthFull
  --   , onDesktopMaxWidth370px
  --   ]
