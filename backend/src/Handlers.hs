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

module Handlers
  ( handlers
  , handleFeedXML
  , mkContext
  ) where

import           AppData                   (DbAction, EnvApplication (..),
                                            Handler)
import           Auth                      (UserInfo (..), mkClaims,
                                            mkCompactJWT, verifyCompactJWT)
import Auth ()
import           Common                    (EpisodeNew (..), RoutesApi,
                                            convertToFilename, formatDuration)
import           Common.Auth               (LoginData (..), SessionData (..),
                                            UserNew (..))
import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Exception.Lifted  (catch, evaluate)
import           Control.Monad             (Monad ((>>=)), unless, when)
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (asks)
import           Control.Monad.Trans       (MonadTrans (lift))
import           Crypto.JWT                (JWK)
import           Data.Aeson                (decodeStrict, encode)
import           Data.Bool                 (Bool (..))
import qualified Data.ByteString.Lazy      as Lazy
import qualified Data.ByteString.Lazy.UTF8 as BSU
import           Data.Char                 (isAlphaNum)
import           Data.Either               (either)
import           Data.Eq                   (Eq ((==)))
import           Data.FileEmbed            (makeRelativeToProject)
import           Data.Foldable             (Foldable (foldl'))
import           Data.Function             (flip, ($))
import           Data.Functor              (Functor (fmap), (<$>))
import           Data.Int                  (Int)
import           Data.List                 (null, sortOn, (++))
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (Just, Nothing), isJust,
                                            maybe)
import           Data.Monoid               ((<>))
import           Data.Ord                  (Down (Down), Ord ((<), (>)))
import           Data.Password             (mkPassword)
import           Data.Password.Argon2      (PasswordCheck (..), checkPassword,
                                            hashPassword)
import           Data.Pool                 (Pool)
import           Data.Text                 (Text, breakOn, drop, replace,
                                            toUpper)
import qualified Data.Text                 as Text
import           Data.Time                 (defaultTimeLocale, formatTime,
                                            getCurrentTime, parseTimeM)
import           Data.Traversable          (traverse)
import           Data.Tuple                (snd)
import           Database.Gerippe          (Entity (..), InnerJoin (..), Key,
                                            PersistStoreWrite (insert, insert_),
                                            PersistUniqueRead (getBy),
                                            PersistentSqlException, from, get,
                                            getAll, getWhere, joinMTo1Where',
                                            on, select, val, where_, (&&.),
                                            (==.), (^.))
import           Database.Persist.MySQL    (PersistStoreWrite (update),
                                            SqlBackend, runSqlPool, (=.))
import           DbAdapter                 (Alias (..), AuthPwd (..),
                                            Clearance (..), EntityField (..),
                                            EventSource (..), Unique (..),
                                            User (..))
import qualified DbAdapter                 as Db
import           Model                     (Episode (..), Event (..),
                                            Journal (..), Platform (..),
                                            Podcast (..), Rank (RankModerator),
                                            Subject (..), Visibility (..))
import           Safe                      (headMay)
import           Servant.API               ((:<|>) (..),
                                            FromHttpApiData (parseHeader))
import           Servant.Server            (Context ((:.), EmptyContext),
                                            HasServer (ServerT),
                                            ServantErr (errBody), err400,
                                            err403, err404, err500, throwError)
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
    \(Entity _ Clearance{..}, Entity _ Db.Podcast{..}) ->
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
          _       -> toServerError $ "user not found: " <> Text.unpack uiUserName
        let uiIsSiteAdmin = userIsSiteAdmin
        clearances <- runDb' pool $
          joinMTo1Where' ClearanceFkPodcast PodcastId
                         ClearanceFkAlias uiKeyAlias
        let uiClearances = Map.fromList $
              for clearances $ \(Entity _ Clearance{..}, Entity _ Db.Podcast{..}) ->
                (podcastIdentifier, clearanceRank)
        pure $ UserInfo{..}
  in  authHandler :. EmptyContext

handlers :: ServerT RoutesApi '[Snap UserInfo] Handler
handlers =
        (handleGrantAuthPwd
    :<|> handleUserNew
    :<|> handleDoesUserExist
        )
  :<|> handleEpisodeNew
  :<|>  (handlePodcastNew
    :<|> handlePodcastGet
        )
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


handleGrantAuthPwd :: LoginData -> Handler (Maybe SessionData)
handleGrantAuthPwd LoginData{..} = do
  mUser <- runDb (getBy $ UUserName ldUserName)
  Entity keyUser User{..} <- maybe
    (throwError $ err400 { errBody = "user does not exist" })
    pure mUser
  mAuth <- runDb (getBy $ UAuthPwdFkUser keyUser)
  Entity _ AuthPwd{..} <- maybe
    (throwError $ err400 { errBody = "not registered with password"})
    pure mAuth
  Entity keyAlias alias <- case userFkDefaultAlias of
    Just key -> runDb (get key) >>= \case
      Just alias -> pure $ Entity key alias
      Nothing    -> throwError $ err500 { errBody = "alias not found" }
    Nothing  -> runDb (getWhere AliasFkUser keyUser) >>= \case
      [alias] -> pure alias
      _       -> throwError $ err500 { errBody = "alias not found" }
  case checkPassword (mkPassword ldPassword) authPwdPassword of
    PasswordCheckSuccess -> do
      now <- liftIO getCurrentTime
      let journalCreated = now
          journalEvent = EventLogin
          journalDescription = ""
          journalSubject = SubjectUser
          blob = Lazy.toStrict $ encode Journal{..}
      runDb $ insert_ $ Db.Journal blob userFkEventSource $ Just keyAlias
      let claims = mkClaims now ldUserName
      jwk <- asks envJwk
      let toServerError e = throwError $ err500 { errBody = BSU.fromString $ show e}
      sdJwt <- liftIO (runExceptT $ mkCompactJWT jwk claims)
        >>= either toServerError pure
      sdClearances <- getClearances keyAlias
      let sdIsSiteAdmin = userIsSiteAdmin
          sdUserName = userName
          sdAliasName = aliasName alias
      pure $ Just SessionData{..}
    PasswordCheckFail    -> pure Nothing

handleUserNew :: UserNew -> Handler SessionData
handleUserNew UserNew{..} = do
  when (Text.null unPassword) $
    throwError $ err400 { errBody = "Password cannot be empty" }
  when (Text.length unUserName > 64) $
    throwError $ err400 { errBody = "User name max length: 64 characters" }
  when (Text.length unPassword > 64) $
    throwError $ err400 { errBody = "Password max length: 64 characters" }
  unless (Text.all isAlphaNum unUserName) $
    throwError $ err400 { errBody = "user name may only contain alpha-numeric characaters" }
  sdIsSiteAdmin <- runDb $ (null :: [Entity User] -> Bool) <$> getAll
  eventSourceId <- runDb $ insert EventSource
  user <- runDb $ insert $ User unUserName sdIsSiteAdmin eventSourceId Nothing
  password <- hashPassword (mkPassword unPassword)
  runDb $ insert_ $ AuthPwd user password
  let sdAliasName = Text.take 16 unUserName
  keyAlias <- runDb $ insert $ Alias sdAliasName user
  runDb $ update user [ UserFkDefaultAlias =. Just keyAlias ]
  now <- liftIO getCurrentTime
  let blobJournalUser =
        let journalCreated = now
            journalEvent = EventCreation
            journalDescription = ""
            journalSubject = SubjectUser
        in  Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal blobJournalUser eventSourceId Nothing
  let blobJournalAlias =
        let journalCreated = now
            journalEvent = EventCreation
            journalDescription = ""
            journalSubject = SubjectAlias
        in  Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal blobJournalAlias eventSourceId Nothing
  jwk <- asks envJwk
  let claims = mkClaims now unUserName
      toServerError e = throwError $ err500 { errBody = BSU.fromString $ show e}
  sdJwt <- liftIO (runExceptT $ mkCompactJWT jwk claims)
    >>= either toServerError pure
  sdClearances <- getClearances keyAlias
  let sdUserName = unUserName
  pure SessionData{..}

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
  fkPodcast <- maybe (throwError $ err500 { errBody = "show not found" })
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
      episodeBlob = Lazy.toStrict $ encode Episode{..}
  runDb $ insert_ $ Db.Episode episodeSlug episodeBlob fkPodcast eventSourceId
  let journalCreated = now
      journalEvent = EventCreation
      journalDescription = ""
      journalSubject = SubjectEpisode
      journalBlob = Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal journalBlob eventSourceId $ Just uiKeyAlias

handleAliasRename :: UserInfo -> Text -> Handler ()
handleAliasRename UserInfo{..} new = do
  when (Text.length new > 16) $
    throwError $ err400 { errBody = "alias max length is 16 characters" }
  now <- liftIO getCurrentTime
  runDb $ update uiKeyAlias [ AliasName =. new ]
  eventSourceId <- runDb (getBy $ UUserName uiUserName) >>=
    maybe (throwError $ err400 { errBody = "user not found"})
          (pure . userFkEventSource . entityVal)
  let journalCreated = now
      journalEvent = EventEdit
      journalDescription = "Old alias: " <> aliasName uiAlias
      journalSubject = SubjectAlias
      journalBlob = Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal journalBlob eventSourceId $ Just uiKeyAlias

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
  eventSourceId <- runDb $ insert EventSource
  let journalCreated = now
      journalEvent = EventCreation
      journalDescription = ""
      journalSubject = SubjectPodcast
      journalBlob = Lazy.toStrict $ encode Journal{..}
  runDb $ insert_ $ Db.Journal journalBlob eventSourceId $ Just uiKeyAlias

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
            es <- traverse (decodeStrict . Db.episodeBlob . entityVal) episodes
            pure (p, ps, es)
      in  maybe (throwError $ err500 { errBody = "Could not decode blobs" })
                pure
                mPodcast
