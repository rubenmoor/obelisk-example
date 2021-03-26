{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Handlers
  ( handlers
  ) where

import           AppData                  (DbAction, EnvApplication (..),
                                           Handler)
import           Auth                     (CompactJWT, Credentials (..),
                                           LoginFailure (..), RespLogin (..))
import           Common                   (EpisodeNew (..), Routes,
                                           convertToFilename, formatDuration)
import           Control.Applicative      (Applicative (pure))
import           Control.Category         (Category ((.)))
import           Control.Exception.Lifted        (evaluate, handle, catch)
import           Control.Monad            (unless, Monad ((>>=)), when)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Control.Monad.Reader     (asks)
import           Data.Bool                (Bool (..))
import qualified Data.ByteString.Lazy     as Lazy
import           Data.Either              (Either (Left, Right))
import           Data.Eq                  (Eq ((==)))
import           Data.FileEmbed           (makeRelativeToProject)
import           Data.Function            (($))
import           Data.Functor             ((<$>))
import           Data.Int                 (Int)
import           Data.List                (sortOn)
import           Data.Maybe               (Maybe (Just, Nothing), maybe)
import           Data.Monoid              ((<>))
import           Data.Ord                 (Down (Down))
import           Data.Text                (Text, breakOn, drop, replace,
                                           toUpper)
import qualified Data.Text                as Text
import           Data.Time                (defaultTimeLocale, formatTime,
                                           getCurrentTime, parseTimeM,
                                           rfc822DateFormat)
import           Data.Tuple               (snd)
import           Database.Gerippe         (PersistStoreWrite (insert, insert_),
                                           PersistentSqlException, getAllValues)
import           Database.Persist.MySQL   (runSqlPool)
import           Model                    (Episode (..), Event (..),
                                           EventSource (..), Journal (..),
                                           Visibility (..))
import qualified Model
import           Safe                     (headMay)
import           Servant.API              ((:<|>) (..))
import           Servant.Server           (err404, err500, HasServer (ServerT),
                                           ServantErr (errBody), err400,
                                           throwError)
import           Text.Blaze.Renderer.Utf8 (renderMarkup)
import           Text.Heterocephalus      (compileHtmlFile)
import Text.Show (Show(show))
import qualified Data.ByteString.Lazy.UTF8 as BSU
import Data.Text.IO (putStrLn)

default(Text)

handlers :: ServerT Routes '[] Handler
handlers =
    handleFeedXML
  :<|>  (handleGrantAuthPwd
    :<|> handleNewUser
    :<|> handleDoesUserExist
        )
  :<|> handleEpisodeNew

runDb :: DbAction a -> Handler a
runDb action = do
  pool <- asks envPool
  catch (liftIO $ runSqlPool action pool >>= evaluate) $
    \(e :: PersistentSqlException) ->
      throwError $ err500 { errBody = BSU.fromString $ show e }

handleFeedXML :: Text -> Handler Lazy.ByteString
handleFeedXML podcastId = do
  unless (podcastId == "fullserendipity.xml") $ throwError err404
  episodeList <- runDb getAllValues
  url <- asks envUrl
  let contents = renderMarkup (
        let title = "full serendipity"
            img = "podcast-logo.jpg"
            imgUrl = url <> img
            description = "Wir reden hier über Themen"
            copyright = "Rubm & Luke"
            email = "luke.rubm@gmail.com (Luke & Rubm)"
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT"
            itunesSubtitle = "Wir reden hier über Themen"
            itunesSummary = "Wir reden hier über Themen"
            authors = "Luke & Rubm"
            itunesOwnerNames = "Luke and Rubm"
            episodeData = getEpisodeFeedData url <$>
              sortOn  (Down . episodeCreated) episodeList
            latestDate = maybe pubDate efdRFC822 $ headMay episodeData
        in  $(makeRelativeToProject "feed.xml.tpl" >>= compileHtmlFile))
  pure contents
  where
    getEpisodeFeedData :: Text -> Model.Episode -> EpisodeFeedData
    getEpisodeFeedData url Model.Episode{..} =
      let efdRFC822 = replace "UTC" "UT" $
            Text.pack $ formatTime defaultTimeLocale rfc822DateFormat episodeCreated
          efdSlug = episodeSlug
          efdFtExtension = episodeFtExtension
          efdAudioFileUrl = mkFileUrl url efdFtExtension efdSlug
          efdPageUrl = url <> efdSlug
          efdTitle = episodeTitle
          efdThumbnailFile = url <>
            if episodeThumbnailFile == "" then "podcast-logo.jpg"
            else Text.pack episodeThumbnailFile
          efdDescription = episodeDescriptionShort
          efdAudioContentType = episodeAudioContentType
          efdDurationSeconds = episodeDuration
          efdDurationFormatted = formatDuration episodeDuration
          efdFileSize = episodeFileSize
      in  EpisodeFeedData{..}

    -- TODO: drop podtrac in favor of custom tracking
    -- https://dts.podtrac.com/redirect.m4a/podcast-static.rubenmoor.net/media/2020-11-15_BANANE.m4a
    mkFileUrl :: Text -> Text -> Text -> Text
    mkFileUrl url filetypeExtension slug =
      let mediaLink' = drop 3 $ snd $ breakOn "://" url
      in     "https://"
          <> "dts.podtrac.com/redirect"
          <> filetypeExtension
          <> "/" <> mediaLink' <> "/"
          <> slug <> filetypeExtension

data EpisodeFeedData = EpisodeFeedData
    { efdRFC822            :: Text
    , efdSlug              :: Text
    , efdFtExtension       :: Text -- filetype extension (.m4a)
    , efdAudioFileUrl      :: Text
    , efdPageUrl           :: Text
    , efdTitle             :: Text
    , efdThumbnailFile     :: Text
    , efdDescription       :: Text
    , efdAudioContentType  :: Text
    , efdDurationSeconds   :: Int
    , efdDurationFormatted :: Text
    , efdFileSize          :: Int
    }


handleGrantAuthPwd :: Credentials -> Handler RespLogin
handleGrantAuthPwd Credentials{..} = pure $ RespLoginFailure LoginFailureWrongPassword

handleNewUser :: Credentials -> Handler (Maybe CompactJWT)
handleNewUser Credentials{..} = pure Nothing

handleDoesUserExist :: Text -> Handler Bool
handleDoesUserExist str = pure False

handleEpisodeNew :: EpisodeNew -> Handler ()
handleEpisodeNew EpisodeNew{..} = do
  now <- liftIO getCurrentTime
  when (newTitle == "") $
    throwError $ err400 { errBody = "title field is mandatory" }
  pubdate <- case parseTimeM False defaultTimeLocale "%F" (Text.unpack newDate) of
    Just d  -> pure d
    Nothing -> throwError $ err400 { errBody = "could not parse date" }
  eventSourceId <- runDb $ insert EventSource
  let day = Text.pack $ formatTime defaultTimeLocale "%F" pubdate
      episodeTitle = newTitle
      episodeSlug = day <> "_" <> convertToFilename (toUpper newTitle)
      episodeCustomIndex = newCustomIndex
      episodeFtExtension = ""
      episodeAudioContentType = ""
      episodeThumbnailFile = ""
      episodeDescriptionShort = ""
      episodeDescriptionLong = ""
      episodePubdate = pubdate
      episodeCreated = now
      episodeDuration = 0
      episodeFileSize = 0
      episodeVideoUrl = ""
      episodeVisibility = VisibilityHidden
      episodeFkEventSource = eventSourceId
  runDb $ insert_ Episode{..}
  let journalFkEventSource = eventSourceId
      journalFkUser = Nothing
      journalCreated = now
      journalEvent = EventCreation
      journalDescription = ""
  runDb $ insert_ Journal{..}
