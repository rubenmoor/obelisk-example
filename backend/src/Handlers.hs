{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
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

import           AppData                   (DbAction, EnvApplication (..),
                                            Handler)
import           Auth                      (UserInfo (..))
import           Common                    (EpisodeNew (..), Routes,
                                            convertToFilename, formatDuration)
import           Common.Auth               (CompactJWT, Credentials (..))
import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Exception.Lifted  (catch, evaluate)
import           Control.Monad             (Monad ((>>=)), when)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (asks)
import           Data.Bool                 (Bool (..))
import qualified Data.ByteString.Lazy      as Lazy
import           Data.Eq                   (Eq ((==)))
import           Data.FileEmbed            (makeRelativeToProject)
import           Data.Function             (($))
import           Data.Functor              ((<$>))
import           Data.Int                  (Int)
import           Data.List                 (map, null, sortOn)
import           Data.Maybe                (Maybe (Just, Nothing), isJust,
                                            maybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (Down (Down))
import           Data.Text                 (Text, breakOn, drop, replace,
                                            toUpper)
import qualified Data.Text                 as Text
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime, parseTimeM)
import           Data.Tuple                (snd)
import           Database.Gerippe          (Entity (..),
                                            PersistStoreWrite (insert, insert_),
                                            PersistUniqueRead (getBy),
                                            PersistentSqlException, getAll,
                                            getWhere)
import           Database.Persist.MySQL    (runSqlPool)
import           Model                     (AuthPwd (..), EntityField (..),
                                            Episode (..), Event (..),
                                            EventSource (..), Journal (..),
                                            Podcast (..), Subject (..),
                                            Unique (..), User (..),
                                            Visibility (..))
                                          -- EventSource (..), Journal (..),
                                          -- Visibility (..))
import qualified Data.ByteString.Lazy.UTF8 as BSU
import           Data.Password             (PasswordCheck (..))
import           Data.Password.Argon2      (PasswordHash (..), checkPassword,
                                            hashPassword, mkPassword)
import           Safe                      (headMay)
import           Servant.API               ((:<|>) (..))
import           Servant.Server            (HasServer (ServerT),
                                            ServantErr (errBody), err400,
                                            err404, err500, throwError)
import           Text.Blaze.Renderer.Utf8  (renderMarkup)
import           Text.Heterocephalus       (compileHtmlFile)
import           Text.Show                 (Show (show))

default(Text)

handlers :: ServerT Routes '[Snap UserInfo] Handler
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
  mShow <- runDb $ getBy $ UPodcastIdentifier podcastId
  theShow <- maybe (throwError err404) pure mShow
  let Entity key Podcast{..} = theShow
  episodeList <- runDb $ map entityVal <$> getWhere EpisodeFkPodcast key
  url <- asks envUrl
  -- TODO: podcastLicence: https://creativecommons.org/licenses/by-nc-nd/4.0/
  -- TODO: podcastKeywords: Philosophie, Moral, Kolumbien
  let podcastUrl = url <> "/" <> podcastId
      imgUrl = podcastUrl <> "/podcast-logo.jpg"
      pubDate = toRfc822 podcastPubDate
      episodeData = getEpisodeFeedData url <$>
        sortOn  (Down . episodeCreated) episodeList
      latestDate = maybe pubDate efdRFC822 $ headMay episodeData
  pure $ renderMarkup $(makeRelativeToProject "feed.xml.tpl" >>= compileHtmlFile)
  where
    getEpisodeFeedData :: Text -> Model.Episode -> EpisodeFeedData
    getEpisodeFeedData url Model.Episode{..} =
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
    toRfc822 =
        replace "UTC" "UT"
      . Text.pack
      . formatTime defaultTimeLocale "%a, %-d %b %Y %H:%M:%S %Z"

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


handleGrantAuthPwd :: Credentials -> Handler (Maybe CompactJWT)
handleGrantAuthPwd Credentials{..} = do -- pure $ RespLoginFailure LoginFailureWrongPassword
  mUser <- runDb (getBy $ UUserName credName)
  user <- maybe (throwError $ err400 { errBody = "user does not exist" }) pure mUser
  mAuth <- runDb (getBy $ UAuthPwdFkUser $ entityKey user)
  Entity _ AuthPwd{..} <- maybe (throwError $ err400 { errBody = "not registered with password"}) pure mAuth
  case checkPassword (mkPassword credPassword) (PasswordHash authPwdPassword) of
    PasswordCheckSuccess -> pure Nothing
    PasswordCheckFail    -> pure Nothing

handleNewUser :: Credentials -> Handler (Maybe CompactJWT)
handleNewUser Credentials{..} = do
  when (Text.null credPassword) $
    throwError $ err400 { errBody = "Password cannot be empty" }
  first <- runDb $ (null :: [Entity User] -> Bool) <$> getAll
  eventSourceId <- runDb $ insert EventSource
  user <- runDb $ insert $ User credName first eventSourceId
  password <- unPasswordHash <$> hashPassword (mkPassword credPassword)
  runDb $ insert_ $ AuthPwd user password
  now <- liftIO getCurrentTime
  let journalFkAlias = Nothing
      journalCreated = now
      journalEvent = EventCreation
      journalDescription = ""
      journalSubject = SubjectUser
      journalFkEventSource = eventSourceId
  runDb $ insert_ Journal{..}
  pure Nothing
  -- insert user
  -- insert authpwd
  -- insert journal

handleDoesUserExist :: Text -> Handler Bool
handleDoesUserExist str = runDb $ isJust <$> getBy (UUserName str)

handleEpisodeNew :: Text -> UserInfo -> EpisodeNew -> Handler ()
handleEpisodeNew theShow UserInfo{..} EpisodeNew{..} = do
  mShow <- runDb $ getBy $ UPodcastIdentifier theShow
  episodeFkPodcast <- maybe (throwError $ err500 { errBody = "show not found" })
                            (pure . entityKey) mShow
  -- TODO: verify clearance
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
      -- TODO: correct alias
      journalFkAlias = Nothing
      journalCreated = now
      journalEvent = EventCreation
      journalDescription = ""
      journalSubject = SubjectEpisode
  runDb $ insert_ Journal{..}
