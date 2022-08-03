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

module Handler.Serendipity
  ( handlers
  ) where

import           AppData                   (Handler)
import           Auth                      (UserInfo (..))
import Auth ()
import           Common.Api                (EpisodeNew (..), RoutesSerendipity)
import           Common                (convertToFilename)
import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Monad             (unless, when)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Data.Aeson                (decodeStrict, encode)
import           Data.Bool                 (Bool (..))
import qualified Data.ByteString.Lazy      as Lazy
import           Data.Char                 (isAlphaNum)
import           Data.Eq                   (Eq ((==)))
import           Data.Foldable             (Foldable (foldl'))
import           Data.Function             (($))
import           Data.List                 ((++))
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (Just, Nothing),
                                            maybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (Ord ((<)))
import           Data.Text                 (Text,
                                            toUpper)
import qualified Data.Text                 as Text
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime, parseTimeM)
import           Data.Traversable          (traverse)
import           Database.Gerippe          (Entity (..), InnerJoin (..), Key,
                                            PersistStoreWrite (insert, insert_),
                                            PersistUniqueRead (getBy), from, get,
                                            getAll, getWhere, joinMTo1Where',
                                            on, select, val, where_, (&&.),
                                            (==.), (^.))
import           DbAdapter                 (Alias (..), EntityField (..),
                                            Unique (..))
import qualified DbAdapter                 as Db
import           Common.Model                     (Episode (..), Platform (..),
                                            Podcast (..), Rank (RankModerator),
                                            Visibility (..), JournalEvent (..), EventApp (..))
import           Servant.API               ((:<|>) (..))
import           Servant.Server            (HasServer (ServerT),
                                            ServantErr (errBody), err400,
                                            err403, err404, err500, throwError)
import           Snap.Core                 (Snap)

import Database (runDb)
import qualified DbJournal
import Data.Functor ((<$>))

default(Text)

handlers :: ServerT RoutesSerendipity '[Snap UserInfo] Handler
handlers =
       handleEpisodeNew
  :<|> handlePodcastNew
  :<|> handlePodcastGet

checkClearance :: UserInfo -> Text -> Rank -> Handler ()
checkClearance UserInfo{..} theShow minRank =
  unless uiIsSiteAdmin $ do
    case Map.lookup theShow uiClearances of
      Just rank -> when (rank < minRank) $ throwError err403
      Nothing   -> throwError err403

handleEpisodeNew :: UserInfo -> Text -> EpisodeNew -> Handler ()
handleEpisodeNew ui@UserInfo{..} theShow EpisodeNew{..} = do
  mShow <- runDb $ getBy $ UPodcastIdentifier theShow
  episodeFkPodcast <- maybe (throwError $ err500 { errBody = "show not found" })
                     (pure . entityKey) mShow
  checkClearance ui theShow RankModerator
  let Alias{..} = uiAlias
  now <- liftIO getCurrentTime
  when (newTitle == "") $
    throwError $ err400 { errBody = "title field is mandatory" }
  pubdate <- case parseTimeM False defaultTimeLocale "%F" (Text.unpack newDate) of
    Just d  -> pure d
    Nothing -> throwError $ err400 { errBody = "could not parse date" }
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
  runDb $ insert_ $ Db.Episode {..}
  DbJournal.insert (Just uiKeyAlias) (EventApp $ EventEpisodeNew newTitle)

handlePodcastGet :: Text -> Handler (Podcast, [Platform], [Episode])
handlePodcastGet podcastIdentifier = do
  ls <- runDb $ select $ from $ \(p `InnerJoin` pl `InnerJoin` e) -> do
    on (p ^. PodcastId ==. pl ^. PlatformFkPodcast)
    on (p ^. PodcastId ==. e ^. EpisodeFkPodcast)
    where_ (p ^. PodcastIdentifier ==. val podcastIdentifier)
    pure (p, (pl, e))
  let insertPair (platforms, episodes) (p, e) = (p ++ platforms, e ++ episodes)
      acc mp (podcast, (p, e)) = Map.insertWith insertPair podcast ([p], [e]) mp
      m = foldl' acc  Map.empty ls
  case Map.toList m of
    []                    -> throwError err404
    _:_:_                 -> throwError $ err500 { errBody = "podcast id not unique" }
    [(podcast, (platforms, episodes))] ->
      let mPodcast = do
            p <- decodeStrict $ Db.podcastBlob $ entityVal podcast
            ps <- traverse (decodeStrict . Db.platformBlob . entityVal) platforms
            let es = Db.fromDbEpisode . entityVal <$> episodes
            pure (p, ps, es)
      in  maybe (throwError $ err500 { errBody = "Could not decode blobs" })
                pure
                mPodcast

handlePodcastNew :: UserInfo -> Text -> Handler ()
handlePodcastNew UserInfo{..} podcastIdentifier = do
  unless uiIsSiteAdmin $ throwError err500
  unless (Text.all isAlphaNum podcastIdentifier) $
    throwError $ err400 { errBody = "only alpha-numeric characters permitted" }
  now <- liftIO getCurrentTime
  let podcastTitle = ""
      podcastDescription = ""
      podcastCopyright = ""
      podcastEmail = ""
      podcastLicence = ""
      podcastPubDate = now
      podcastItunesSubtitle = ""
      podcastItunesSummary = ""
      podcastAuthors = ""
      podcastItunesOwnerNames = ""
      podcastKeywords = ""
      podcastBlob = Lazy.toStrict $ encode Podcast{..}
  runDb $ insert_ $ Db.Podcast podcastIdentifier podcastBlob
  DbJournal.insert (Just uiKeyAlias) (EventApp $ EventPodcastNew podcastIdentifier)
