{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TupleSections #-}
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
  , mkContext
  ) where

import           AppData                   (DbAction, EnvApplication (..),
                                            Handler)
import           Auth                      (mkClaims, mkCompactJWT, verifyCompactJWT)
import           Common                    (EpisodeNew (..), Routes,
                                            convertToFilename, formatDuration)
import           Common.Auth               (CompactJWT, LoginData (..),
                                            UserInfo (..), UserNew (..))
import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Exception.Lifted  (catch, evaluate)
import           Control.Monad             (unless, Monad ((>>=)), when)
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans       (MonadTrans (lift))
import           Crypto.JWT                (JWK)
import           Data.Bool                 (Bool (..))
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy.UTF8 as BSU
import           Data.Either               (either)
import           Data.Eq                   (Eq ((==)))
import           Data.FileEmbed            (makeRelativeToProject)
import           Data.Function             (flip, ($))
import           Data.Functor
import           Data.Int                  (Int)
import           Data.List                 (map, null, sortOn)
import qualified Data.Map                  as Map
import Data.Map (Map)
import           Data.Maybe                (Maybe (Just, Nothing), isJust,
                                            maybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (Ord((<)), Down (Down))
import           Data.Password             (mkPassword)
import           Data.Password.Argon2      (PasswordCheck (..), checkPassword,
                                            hashPassword)
import           Data.Pool                 (Pool)
import           Data.String               (String)
import           Data.Text                 (Text, breakOn, drop, replace,
                                            toUpper)
import qualified Data.Text                 as Text
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime, parseTimeM)
import           Data.Tuple                (snd)
import           Database.Gerippe          (joinMTo1Where, join1ToMWhere, Entity (..), InnerJoin (..),
                                            PersistStoreWrite (insert, insert_),
                                            PersistUniqueRead (getBy),
                                            PersistentSqlException, from,
                                            getAll, getWhere, joinMTo1Where',
                                            on, select, val, where_, (&&.),
                                            (==.), (^.), get, Key)
import           Database.Persist.MySQL    ((=.), PersistStoreWrite(update), SqlBackend, runSqlPool)
import           Model                     (Rank(RankModerator), Alias (..), AuthPwd (..),
                                            Clearance (..), EntityField (..),
                                            Episode (..), Event (..),
                                            EventSource (..), Journal (..),
                                            Podcast (..), Subject (..),
                                            Unique (..), User (..),
                                            Visibility (..))
import           Safe                      (headMay)
import           Servant.API               ((:<|>) (..),
                                            FromHttpApiData (parseHeader))
import           Servant.Server            (err403, Context ((:.), EmptyContext),
                                            HasServer (ServerT),
                                            ServantErr (errBody), err400,
                                            err404, err500, throwError)
import           Snap.Core                 (Snap, getHeader, getRequest)
import           Text.Blaze.Renderer.Utf8  (renderMarkup)
import           Text.Heterocephalus       (compileHtmlFile)
import           Text.Show                 (Show (show))

default(Text)

getClearances :: Key Alias -> Handler (Map Text Rank)
getClearances keyAlias = do
  clearances <- runDb $
    joinMTo1Where' ClearanceFkPodcast PodcastId
                   ClearanceFkAlias keyAlias
  let for = flip fmap
  pure $ Map.fromList $ for clearances $
    \(Entity _ Clearance{..}, Entity _ Podcast{..}) ->
      (podcastIdentifier, clearanceRank)

mkContext :: JWK -> Pool SqlBackend -> Context '[Snap UserInfo]
mkContext jwk pool =
  let authHandler :: Snap UserInfo
      authHandler = do
        let toServerError e = throwError $ err500 { errBody = BSU.fromString e}
            for = flip fmap
        mAuth <- getHeader "Authorization" <$> getRequest
        auth <- maybe (toServerError "authorization header missing")
                pure mAuth
        mAlias <- getHeader "X-Alias" <$> getRequest
        aliasBs <- maybe (toServerError "X-Alias header missing")
                   pure mAlias
        uiAliasName <- either (\s -> toServerError $ "parseHeader alias: " <> show s)
                       pure $ parseHeader aliasBs
        jwt <- either (\s -> toServerError $ "parseHeader jwt: " <> show s)
               pure $ parseHeader auth
        eSub <- liftIO $ runExceptT $ verifyCompactJWT jwk jwt
        uiUserName <- either (\s -> toServerError $ "verify jwt: " <> show s)
                      pure eSub
        ls <- runDb' pool $ select . from $ \(a `InnerJoin` u) -> do
          on     $ a ^. AliasFkUser ==. u ^. UserId
          where_ $ u ^. UserName ==. val uiUserName
               &&. a ^. AliasName ==. val uiAliasName
          pure (u, a)
        (Entity _ User{..}, Entity uiKeyAlias uiAlias) <- case ls of
          [entry] -> pure entry
          _       -> toServerError ("user not found" :: String)
        let uiIsSiteAdmin = userIsSiteAdmin
        clearances <- runDb' pool $
          joinMTo1Where' ClearanceFkPodcast PodcastId
                         ClearanceFkAlias uiKeyAlias
        let uiClearances = Map.fromList $
              for clearances $ \(Entity _ Clearance{..}, Entity _ Podcast{..}) ->
                (podcastIdentifier, clearanceRank)
        pure $ UserInfo{..}
  in  authHandler :. EmptyContext

handlers :: ServerT Routes '[Snap UserInfo] Handler
handlers =
    handleFeedXML
  :<|>  (handleGrantAuthPwd
    :<|> handleUserNew
    :<|> handleDoesUserExist
        )
  :<|> handleEpisodeNew
  :<|>  (handleAliasRename
    :<|> handleAliasGetAll
    :<|> handleAliasSetDefault
        )

runDb :: DbAction a -> Handler a
runDb action = do
  pool <- asks envPool
  lift $ runDb' pool action

runDb' :: Pool SqlBackend -> DbAction a -> Snap a
runDb' pool action =
  catch (liftIO $ runSqlPool action pool >>= evaluate) $
    \(e :: PersistentSqlException) ->
      throwError $ err500 { errBody = BSU.fromString $ "db error: " <> show e }

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


handleGrantAuthPwd :: LoginData -> Handler (Maybe (CompactJWT, UserInfo))
handleGrantAuthPwd LoginData{..} = do
  mUser <- runDb (getBy $ UUserName ldUserName)
  Entity keyUser User{..} <- maybe
    (throwError $ err400 { errBody = "user does not exist" })
    pure mUser
  mAuth <- runDb (getBy $ UAuthPwdFkUser keyUser)
  Entity _ AuthPwd{..} <- maybe
    (throwError $ err400 { errBody = "not registered with password"})
    pure mAuth
  Entity uiKeyAlias uiAlias <- case userFkDefaultAlias of
    Just key -> runDb (get key) >>= \case
      Just alias -> pure $ Entity key alias
      Nothing    -> throwError $ err500 { errBody = "alias not found" }
    Nothing  -> runDb (getWhere AliasFkUser keyUser) >>= \case
      [alias] -> pure alias
      _       -> throwError $ err500 { errBody = "alias not found" }
  case checkPassword (mkPassword ldPassword) authPwdPassword of
    PasswordCheckSuccess -> do
      now <- liftIO getCurrentTime
      runDb $ insert_ $
        let journalFkAlias = Just uiKeyAlias
            journalCreated = now
            journalEvent = EventLogin
            journalDescription = ""
            journalSubject = SubjectUser
            journalFkEventSource = userFkEventSource
        in  Journal{..}
      let claims = mkClaims now ldUserName
      jwk <- asks envJwk
      eJwt <- liftIO $ runExceptT $ mkCompactJWT jwk claims
      uiClearances <- getClearances uiKeyAlias
      let uiIsSiteAdmin = userIsSiteAdmin
          uiUserName = userName
      let ui = UserInfo{..}
      let toServerError e = throwError $ err500 { errBody = BSU.fromString $ show e}
      either toServerError (\jwt -> pure $ Just (jwt, ui)) eJwt
    PasswordCheckFail    -> pure Nothing

handleUserNew :: UserNew -> Handler (CompactJWT, UserInfo)
handleUserNew UserNew{..} = do
  when (Text.null unPassword) $
    throwError $ err400 { errBody = "Password cannot be empty" }
  uiIsSiteAdmin <- runDb $ (null :: [Entity User] -> Bool) <$> getAll
  eventSourceId <- runDb $ insert EventSource
  user <- runDb $ insert $ User unUserName uiIsSiteAdmin eventSourceId Nothing
  password <- hashPassword (mkPassword unPassword)
  runDb $ insert_ $ AuthPwd user password
  let uiAlias = Alias unUserName user
  uiKeyAlias <- runDb $ insert uiAlias
  runDb $ update user [ UserFkDefaultAlias =. Just uiKeyAlias ]
  now <- liftIO getCurrentTime
  runDb $ insert_ $
    let journalFkAlias = Nothing
        journalCreated = now
        journalEvent = EventCreation
        journalDescription = ""
        journalSubject = SubjectUser
        journalFkEventSource = eventSourceId
    in  Journal{..}
  runDb $ insert_ $
    let journalFkAlias = Nothing
        journalCreated = now
        journalEvent = EventCreation
        journalDescription = ""
        journalSubject = SubjectAlias
        journalFkEventSource = eventSourceId
    in  Journal{..}
  let claims = mkClaims now unUserName
  jwk <- asks envJwk
  eJwt <- liftIO $ runExceptT $ mkCompactJWT jwk claims
  uiClearances <- getClearances uiKeyAlias
  let uiUserName = unUserName
  let ui = UserInfo{..}
  let toServerError e = throwError $ err500 { errBody = BSU.fromString $ show e}
  either toServerError (pure . (, ui)) eJwt

handleDoesUserExist :: Text -> Handler Bool
handleDoesUserExist str = runDb $ isJust <$> getBy (UUserName str)

checkClearance :: UserInfo -> Text -> Rank -> Handler ()
checkClearance UserInfo{..} theShow minRank =
  unless uiIsSiteAdmin $ do
    case Map.lookup theShow uiClearances of
      Just rank -> when (rank < minRank) $ throwError err403
      Nothing   -> throwError err403

handleEpisodeNew :: Text -> UserInfo -> EpisodeNew -> Handler ()
handleEpisodeNew theShow ui@UserInfo{..} EpisodeNew{..} = do
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
      journalFkAlias = Just uiKeyAlias
      journalCreated = now
      journalEvent = EventCreation
      journalDescription = ""
      journalSubject = SubjectEpisode
  runDb $ insert_ Journal{..}

handleAliasRename :: UserInfo -> Text -> Handler ()
handleAliasRename UserInfo{..} new =
  runDb $ update uiKeyAlias [ AliasName =. new ]

handleAliasGetAll :: UserInfo -> Handler [Text]
handleAliasGetAll UserInfo{..} = do
  mUser <- runDb $ getBy $ UUserName uiUserName
  keyUser <- maybe (throwError $ err500 { errBody = "user not found" })
                   (pure . entityKey)
                   mUser
  ls <- runDb $ getWhere AliasFkUser keyUser
  pure $ aliasName . entityVal <$> ls

handleAliasSetDefault :: UserInfo -> Text -> Handler ()
handleAliasSetDefault UserInfo{..} aliasName = do
  keyAlias <- runDb (getBy $ UAliasName aliasName)
    >>= maybe (throwError $ err500 { errBody = "alias name not found" })
              (pure . entityKey)
  keyUser <- runDb (getBy $ UUserName uiUserName)
    >>= maybe (throwError $ err500 { errBody = "user not found" })
              (pure . entityKey)
  runDb $ update keyUser [ UserFkDefaultAlias =. Just keyAlias ]
