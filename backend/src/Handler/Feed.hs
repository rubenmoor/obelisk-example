{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Handler.Feed
  ( handleFeedXML
  ) where

import           AppData                   (EnvApplication (..),
                                            Handler)
import Auth ()
import           Common                (formatDuration)
import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)))
import           Control.Monad.Reader      (asks)
import           Data.Aeson                (decodeStrict)
import qualified Data.ByteString.Lazy      as Lazy
import           Data.Eq                   (Eq ((==)))
import           Data.FileEmbed            (makeRelativeToProject)
import           Data.Function             (($))
import           Data.Functor              ((<$>))
import           Data.Int                  (Int)
import           Data.List                 (sortOn)
import           Data.Maybe                (maybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (Down (Down))
import           Data.Text                 (Text, breakOn, drop, replace)
import qualified Data.Text                 as Text
import           Data.Time                 (defaultTimeLocale, formatTime)
import           Data.Traversable          (traverse)
import           Data.Tuple                (snd)
import           Database.Gerippe          (Entity (..), InnerJoin (..), Key,
                                            PersistStoreWrite (insert, insert_),
                                            PersistUniqueRead (getBy), from, get,
                                            getAll, getWhere, joinMTo1Where',
                                            on, select, val, where_, (&&.),
                                            (==.), (^.))
import           DbAdapter                 (EntityField (..),
                                            Unique (..))
import qualified DbAdapter                 as Db
import           Common.Model                     (Episode (..),
                                            Podcast (..))
import           Safe                      (headMay)
import           Servant.Server            (ServantErr (errBody), err404, err500, throwError)
import           Text.Blaze.Renderer.Utf8  (renderMarkup)
import           Text.Heterocephalus       (compileHtmlFile)

import Database (runDb)

default(Text)

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

handleFeedXML :: Text -> Handler Lazy.ByteString
handleFeedXML podcastId = do
  Entity key dbPodcast <- runDb (getBy $ UPodcastIdentifier podcastId)
    >>= maybe (throwError err404) pure
  Podcast{..} <- maybe (throwError $ err500 { errBody = "could not decode podcast blob" })
                       pure $ decodeStrict (Db.podcastBlob dbPodcast)
  ls <- runDb (getWhere EpisodeFkPodcast key)
  let mEpisodeList = traverse (decodeStrict . Db.episodeBlob . entityVal) ls
  episodeList <- maybe (throwError $ err500 { errBody = "Could not decode episode blob" })
                       pure
                       mEpisodeList
  url <- asks envUrl
  -- TODO: podcastLicence: https://creativecommons.org/licenses/by-nc-nd/4.0/
  -- TODO: podcastKeywords: Philosophie, Moral, Kolumbien
  let podcastUrl = url <> "/show/" <> podcastId
      imgUrl = podcastUrl <> "/podcast-logo.jpg"
      pubDate = toRfc822 podcastPubDate
      episodeData = getEpisodeFeedData url <$>
        sortOn  (Down . episodeCreated) episodeList
      latestDate = maybe pubDate efdRFC822 $ headMay episodeData
  pure $ renderMarkup $(makeRelativeToProject "feed.xml.tpl" >>= compileHtmlFile)
  where
    getEpisodeFeedData :: Text -> Episode -> EpisodeFeedData
    getEpisodeFeedData url Episode{..} =
      let efdRFC822 = toRfc822 episodeCreated
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

    mkFileUrl :: Text -> Text -> Text -> Text
    mkFileUrl url filetypeExtension slug =
      let mediaLink' = drop 3 $ snd $ breakOn "://" url
      in     "https://"
          <> "dts.podtrac.com/redirect"
          <> filetypeExtension
          <> "/" <> mediaLink' <> "/"
          <> slug <> filetypeExtension
    toRfc822 =
        replace "UTC" "UT"
      . Text.pack
      . formatTime defaultTimeLocale "%a, %-d %b %Y %H:%M:%S %Z"
