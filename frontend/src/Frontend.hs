{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Frontend where

import qualified Data.Map                  as Map
import qualified Data.Text                 as Text

import           Obelisk.Frontend          (Frontend (..), ObeliskWidget)
import           Obelisk.Generated.Static  (static)
import           Obelisk.Route             (R)

import           Reflex.Dom.Core           (InputElement (..), blank, button,
                                            el, elAttr,
                                            inputElementConfig_setValue, text)

import           Clay                      (paddingBottom, Size, maxWidth, marginBottom, borderRadius, zIndex, top, left, translate, transform, absolute, right, block, Auto(auto), important, Center (center), Color, Css,
                                            None (none), Selector, after,
                                            backgroundColor, body, border,
                                            borderBox, both, bottom, boxSizing,
                                            clear, color, compact, content,
                                            darkgray, display, displayTable,
                                            fixed, float, floatLeft, floatRight,
                                            fontFamily, fontSize, fontStyle,
                                            gray, height, inline, italic,
                                            lightgray, margin, padding, pct,
                                            position, pt, px, queryOnly, red,
                                            renderSelector, renderWith, rgb,
                                            sansSerif, solid, star,
                                            stringContent, textAlign, white,
                                            width, (#), (?), (^=))
import           Clay.Media                (screen)
import qualified Clay.Media                as Media
import           Clay.Render               (htmlInline)
import           Client                    (postEpisodeNew)
import           Common                    (EpisodeNew (..), convertToFilename)
import           Control.Applicative       (Applicative (pure, (<*>)))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)), mapM_)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Data.Bool                 (not, (&&), (||), bool, Bool(True, False))
import           Data.Default              (Default (def))
import           Data.Either               (Either (..))
import           Data.Foldable             (traverse_)
import           Data.Function             (const, flip, ($), (&))
import           Data.Functor              (void, Functor (fmap), ($>), (<$>))
import           Data.Map                  (Map)
import           Data.Maybe                (Maybe (..), fromMaybe, maybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, toUpper, unwords)
import qualified Data.Text.Lazy            as Lazy
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime)
import           Data.Tuple                (fst, snd)
import           Data.Witherable           (mapMaybe)
import           Obelisk.Route.Frontend    (RoutedT)
import           Reflex.Dom                (AttributeName, foldDyn, Prerender(prerender), elAttr', el', foldDynMaybe, attachPromptlyDynWithMaybe, attachPromptlyDynWith, leftmost, elDynAttr', DomBuilder (DomBuilderSpace, inputElement),
                                            Element, EventName (Click),
                                            EventResult, HasDomEvent (domEvent),
                                            MonadHold (holdDyn),
                                            PerformEvent (performEvent),
                                            PostBuild (getPostBuild), constDyn,
                                            dynText, elClass, elClass',
                                            elDynAttr,
                                            elementConfig_initialAttributes,
                                            inputElementConfig_elementConfig,
                                            prerender_, toggle, (.~), (=:))
import           Route                     (FrontendRoute)
import           Servant.Common.Req        (reqFailure)
import GHC.Num (Num((-)))


for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

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
  mkResponsiveClass desktopOnly (important $ display block) ".onDesktopDisplayImportant"

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

onDesktopBorder :: ResponsiveClass
onDesktopBorder =
  mkResponsiveClass desktopOnly navBorder ".onDesktopBorder"

onMobileFontBig :: ResponsiveClass
onMobileFontBig =
  mkResponsiveClass mobileOnly (fontSize $ pt 40) ".onMobileFontBig"

onMobileMkOverlay :: ResponsiveClass
onMobileMkOverlay =
  mkResponsiveClass mobileOnly (do -- width $ pct 100
                                   height $ pct 100
                                   top $ px 0
                                   important $ paddingBottom $ px 100
                               ) ".onMobileMkOverlay"

onDesktopMkOverlay :: ResponsiveClass
onDesktopMkOverlay =
  mkResponsiveClass desktopOnly
    (do position absolute
        top (pct 50)
        left (pct 50)
        transform (translate (pct $ -50) $ pct $ -50)
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

navBorder :: Css
navBorder = border solid (px 1) lightgray

elLabelInput conf label id = do
  elAttr "label" ("for" =: id) $ el "h3" $ text label
  let css = do
        fontSize (pt 24)
        padding (px 8) (px 8) (px 8) (px 8)
        borderRadius (px 12) (px 12) (px 12) (px 12)
        border solid (px 1) gray
        marginBottom (px 12)
  i <- inputElement $ conf
         & inputElementConfig_elementConfig
         . elementConfig_initialAttributes
         .~ ("id" =: id <> styleA css <> "class" =: rcClass onMobileWidthFull)
  let str = _inputElement_value i
  pure $ fmap (\s -> if Text.null s then Nothing else Just s) str

htmlBody
  :: forall t js (m :: * -> *)
  . (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
htmlBody = do

  -- navigation
  let cssNav = do
        textAlign center
        backgroundColor white
        navBorder
      divNavBar = elAttr "div" ("class" =: unwords
        [ "row"
        , rcClass onMobileAtBottom
        , rcClass onMobileFontBig
        ] <> style (do textAlign center
                       backgroundColor white
                       zIndex 2
                   ))
  (eLogin, eRegister) <- divNavBar $ mdo
    let navButtonAttrs = for displayNav $ \d ->
             "class" =: unwords [ "col-2"
                                , rcClass onMobileHeight80
                                , rcClass onDesktopDisplayImportant
                                ]
          <> style (do cssNav
                       display d
                       zIndex 2
                   )
        spanNavBtn = fmap fst . elDynAttr' "span" navButtonAttrs
    elLogin <- spanNavBtn $ text "Login"
    elRegister <- spanNavBtn $ text "Register"

    let spanNavBtnHome = fmap fst . elAttr' "span"
          ("class" =: unwords [ "col-2"
                              , rcClass onMobileWidthAuto
                              , rcClass onDesktopBorder
                              ])
    elHome <- spanNavBtnHome $ do
      iFa "fas fa-home"
      elAttr "span" (respClass onMobileDisplayNone) $ text "Home"

    let spanNavBars = elAttr "span" (respClasses [onDesktopDisplayNone]
                                   <> style (do position absolute
                                                right (px 0)
                                                padding (px 0) (px 8) (px 0) (px 8)
                                            ))
    elBars <- spanNavBars $ iFa' "fas fa-bars"

    let eLogin = domEvent Click elLogin
        eRegister = domEvent Click elRegister
    let eToggle = domEvent Click elBars
        toggleFunc True  s = Just $ not s -- toggle btn always toggles
        toggleFunc False True = Just False -- other btns only hide
        toggleFunc False False = Nothing
    dynToggle <- foldDynMaybe toggleFunc False $
      leftmost [eToggle $> True, eLogin $> False, eRegister $> False]
    let displayNav = bool none block <$> dynToggle
    pure (eLogin, eRegister)

  mdo
    -- registration / user new
    let overlayAttrs = for displayOverlay $ \d ->
          respClasses [ onMobileMkOverlay
                      , onDesktopMkOverlay
                      ] <>
          style (do backgroundColor white
                    padding (px 24) (px 24) (px 24) (px 24)
                    position absolute
                    zIndex 1
                    display d
                    )
        divOverlay = elDynAttr "div" overlayAttrs
    (elClose, userName, password) <- divOverlay $ do
      let spanClose = elAttr' "span" (style $ float floatRight) $ iFa "fas fa-times"
          divFieldDescription = elAttr "div" $ respClass onDesktopMaxWidth370px
      elClose <- fst <$> spanClose
      userName <- elLabelInput def "Username" "username"
      divFieldDescription $ text "Your user name is not publicly visible. You \
                                 \can choose your public alias in the next step."
      password <- elLabelInput def "Password" "password"
      divFieldDescription $ text "You enter your password only once. There are \
                                 \no invalid passwords except for an empty one.\
                                 \ Password reset via email can be optionally added later."
      pure (elClose, userName, password)
    let eClose = domEvent Click elClose
    dynShowOverlay <- holdDyn False $ leftmost [eRegister $> True, eClose $> False]
    let displayOverlay = bool none block <$> dynShowOverlay
    blank

  el "h1" $ text "Create new episode"
  pb <- getPostBuild
  postBuildTime <- performEvent $ pb $> liftIO getCurrentTime
  let today = Text.pack . formatTime defaultTimeLocale "%F" <$> postBuildTime
  date <- elLabelInput (def & inputElementConfig_setValue .~ today)
                       "Episode date: " "date"
  customIndex <- elLabelInput def "Custom index: " "customIndex"
  title <- elLabelInput def "Episode title: " "title"

  text "Title: "
  el "br" blank

  -- dynamic title
  let cfg = do
        mc <- customIndex
        mt <- title
        let mTitle = do
              c <- mc
              t <- mt
              pure (Map.empty, "#" <> c <> " " <> t)
        pure $ fromMaybe (style $ fontStyle italic, "empty") mTitle
  elDynAttr "span" (fst <$> cfg) $ dynText (snd <$> cfg)

  el "br" blank
  text "Episode slug (no special chars allowed): "
  el "br" blank
  dynText $ do mt <- title
               let str = (convertToFilename . toUpper . fromMaybe "") mt
               md <- date
               pure $ fromMaybe "" md <> "-" <> str

  el "br" blank
  sendButton <- button "send"

  let eitherEpisodeNew = do
        mCustomIndex <- customIndex
        mTitle       <- title
        mDate        <- date
        let mEpisodeNew = EpisodeNew <$> mCustomIndex <*> mTitle <*> mDate
        pure $ maybe (Left "all fields are required") Right mEpisodeNew
  -- with auth
  -- res <- postEpisodeNew (constDyn $ Left "no jwt") eitherEpisodeNew sendButton
  elAttr "p" (style $ color red) $ prerender_ (text "loading") $ do
    -- TODO: make client aware of currently selected show
    res <- lift $ postEpisodeNew (constDyn $ Right "fullserendipity")
                                 (constDyn $ Left "no jwt")
                                 eitherEpisodeNew
                                 sendButton
    let err = mapMaybe reqFailure res
    holdDyn "" err >>= dynText

  el "h1" $ text "Welcome to Obelisk!"
  el "p" $ text "foooooo"
  pure ()

htmlHead
  :: forall t js (m :: * -> *)
  .  (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
htmlHead = do
  elAttr "meta" ( "content" =: "text/html;charset=utf-8"
               <> "httpequiv" =: "content-type"
                ) blank
  elAttr "meta" ( "content" =: "utf-8"
               <> "httpequiv" =: "encoding"
                ) blank
  elAttr "meta" ( "name" =: "viewport"
               <> "content" =: "width=device-width, initial-scale=1.0"
                ) blank
  elAttr "link" ( "rel" =: "preconnect"
               <> "href" =: "https://fonts.gstatic.com"
                ) blank
  elAttr "link" ( "href" =: "https://fonts.googleapis.com/css2?family=Abel&display=swap"
               <> "rel" =: "stylesheet"
                ) blank
  -- Font Awesome 5.13 free content -->
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: static @"FontAwesome/css/all.min.css"
                ) blank
  el "title" $ text "serendipity.works"
  el "style" $ text $ Lazy.toStrict $ renderWith compact [] $ do
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
    traverse_ rcCss
      [ onMobileDisplayNone
      , onDesktopDisplayNone
      , onMobileFloatLeft
      , onMobileFloatRight
      , onMobileAtBottom
      , onMobileHeight80
      , onDesktopDisplayImportant
      , onMobileWidthAuto
      , onDesktopBorder
      , onMobileFontBig
      , onMobileMkOverlay
      , onDesktopMkOverlay
      , onMobileWidthFull
      , onDesktopMaxWidth370px
      ]


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHead
  , _frontend_body = htmlBody
  }
