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

import           Clay                      (important, Center (center), Color, Css,
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
import           Data.Bool                 (bool, Bool(True, False))
import           Data.Default              (Default (def))
import           Data.Either               (Either (..))
import           Data.Foldable             (traverse_)
import           Data.Function             (flip, ($), (&))
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
import           Reflex.Dom                (DomBuilder (DomBuilderSpace, inputElement),
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


for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

anthrazit :: Color
anthrazit = rgb 8 20 48 -- #081430;

style :: Css -> Map Text Text
style css = "style" =: Lazy.toStrict (renderWith htmlInline [] css)

elClassStyle :: DomBuilder t m => Text -> Text -> Css -> m a -> m a
elClassStyle el' class' css inner =
  let attrs = "class" =: class'
           <> style css
  in  elAttr el' attrs inner

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
  mkResponsiveClass desktopOnly (important $ display inline) ".onDesktopDisplayImportant"

onMobileAtBottom :: ResponsiveClass
onMobileAtBottom =
  let css = do
        position fixed
        bottom (pct 0)
  in  mkResponsiveClass mobileOnly css ".onMobileAtBottom"

onMobileHeight80 :: ResponsiveClass
onMobileHeight80 =
  mkResponsiveClass mobileOnly (height $ px 80) ".onMobileHeight80"

classes :: [Text] -> Map Text Text
classes ls = "class" =: unwords ls

respClasses :: [ResponsiveClass] -> Map Text Text
respClasses = classes . fmap rcClass

respClass :: ResponsiveClass -> Map Text Text
respClass rc = "class" =: rcClass rc

htmlBody
  :: forall t js (m :: * -> *)
  . (ObeliskWidget js t (R FrontendRoute) m)
  => RoutedT t (R FrontendRoute) m ()
htmlBody = mdo
  -- navigation
  let cssNav = do
        textAlign center
        backgroundColor white
      navButtonAttrs = for showNav $ \d ->
           "class" =: unwords [ "col-2"
                              , rcClass onMobileHeight80
                              , rcClass onDesktopDisplayImportant
                              ]
        <> style (do cssNav
                     display d
                 )
      navButton inner = elDynAttr "div" navButtonAttrs inner
  showNav <- elAttr "div" ("class" =: unwords ["row", rcClass onMobileAtBottom]) $ do
    navButton $ text "Login"
    navButton $ text "Register"
    elAttr "div" ("class" =: unwords ["col-2", rcClass onMobileHeight80]
               <> style cssNav) $ do
      e <- elAttr "span" (respClasses [onMobileFloatRight, onDesktopDisplayNone]
                    ) $ iFa' "fas fa-bars"
      elAttr "span" (respClass onMobileFloatLeft) $ do
        iFa "fas fa-home"
        elAttr "span" (respClass onMobileDisplayNone) $ text "Home"
      dynToggle <- toggle False (domEvent Click e)
      pure $ bool none inline <$> dynToggle


  btnLogin <- el "div" $ button "Login"

  let input conf label id = do
        elAttr "label" ("for" =: id) $ text label
        el "br" blank
        i <- inputElement $ conf
               & inputElementConfig_elementConfig
               . elementConfig_initialAttributes .~ ("id" =: id)
        el "br" blank
        let str = _inputElement_value i
        pure $ fmap (\s -> if Text.null s then Nothing else Just s) str

  el "h1" $ text "Create new episode"
  pb <- getPostBuild
  postBuildTime <- performEvent $ pb $> liftIO getCurrentTime
  let today = Text.pack . formatTime defaultTimeLocale "%F" <$> postBuildTime
  date <- input (def & inputElementConfig_setValue .~ today
                ) "Episode date: " "date"
  customIndex <- input def "Custom index: " "customIndex"
  title <- input def "Episode title: " "title"

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
      fontSize (pt 42)
      color anthrazit
      backgroundColor lightgray
      margin (px 0) (px 0) (px 0) (px 0)
    star ? boxSizing borderBox
    star # ("class" ^= "col-") ? do
      float floatRight
      padding (px 5) (px 5) (px 5) (px 5)
      border solid (px 1) lightgray
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
      ]


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHead
  , _frontend_body = htmlBody
  }
