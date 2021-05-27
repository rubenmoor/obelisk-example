{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import           Route                           (BackendRoute (..),
                                                  FrontendRoute,
                                                  fullRouteEncoder)

import           AppData                         (EnvApplication (..))
import           Auth                            (UserInfo)
import           Common                          (RouteShow,
                                                  RoutesApi)
import           Config                          (Params (..))
import           Control.Applicative             (Applicative (pure))
import           Control.Monad.Logger            (NoLoggingT (..))
import           Control.Monad.Reader            (ReaderT (runReaderT))
import           Control.Monad.Trans             (MonadIO (liftIO))
import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.Aeson                      (FromJSON)
import qualified Data.Aeson                      as Aeson
import           Data.Function                   (($))
import           Data.Maybe                      (maybe)
import           Data.Monoid                     ((<>))
import           Data.Proxy                      (Proxy (Proxy))
import           Data.Text                       (Text)
import           Data.Text.IO                    (putStrLn)
import           Database.MySQL.Base.Types       (Option (CharsetName))
import           Database.Persist.MySQL          (ConnectInfo (..),
                                                  defaultConnectInfo,
                                                  runMigration, runSqlPool,
                                                  withMySQLPool)
import           DbAdapter                       (migrateAll)
import           Handlers                        (handleFeedXML, handlers,
                                                  mkContext)
import           Obelisk.Backend                 (Backend (..))
import           Obelisk.Configs                 (getTextConfig, ConfigsT,
                                                  HasConfigs (getConfig),
                                                  runConfigsT)
import           Obelisk.ExecutableConfig.Lookup (getConfigs)
import           Obelisk.Route                   (pattern (:/))
import           Servant                         (serveDirectory)
import           Servant.Server                  (Context, serveSnap,
                                                  serveSnapWithContext)
import           Snap.Core                       (Snap)
import           System.Exit                     (ExitCode (ExitFailure),
                                                  exitWith)
import           System.IO                       (IO)

serveApi :: Context '[Snap UserInfo] -> EnvApplication -> Snap ()
serveApi ctx = runReaderT $ serveSnapWithContext (Proxy :: Proxy RoutesApi) ctx handlers

serveFeed :: EnvApplication -> Snap ()
serveFeed = runReaderT $ serveSnap (Proxy :: Proxy RouteShow) handleFeedXML

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    cfgs <- getConfigs
    (Params{..}, jwk, url) <- runConfigsT cfgs $ do
      params <- getConfigOrExit "backend/params"
      jwk <- getConfigOrExit "backend/jwk"
      mUrl <- getTextConfig "common/route"
      url <- maybe (exitWithFailure "config common/route not found") pure mUrl
      pure (params, jwk, url)
    let connectInfo = defaultConnectInfo
          { connectHost = paramDbHost
          , connectUser = paramDbUser
          , connectPassword = paramDbPassword
          , connectDatabase = paramDbDatabase
          , connectOptions = [ CharsetName "utf8mb4" ]
          }
    runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
      -- TODO: why runResourceT and not liftIO?
      runResourceT $ runSqlPool (runMigration migrateAll) pool
      let env = EnvApplication
            { envPool      = pool
            , envMediaDir  = paramMediaDir
            , envUrl       = url
            , envJwk       = jwk
            }
          ctx = mkContext jwk pool
      NoLoggingT $ serve $ \case
        (BackendRoute_Missing :/ _) -> pure ()
        (BackendRoute_Show    :/ _) -> serveFeed env
        (BackendRoute_Media   :/ _) -> serveDirectory "media"
        (BackendRoute_Api     :/ _) -> serveApi ctx env
  , _backend_routeEncoder = fullRouteEncoder
  }

getConfigOrExit :: forall a. FromJSON a => Text -> ConfigsT IO a
getConfigOrExit filename = do
    mStr <- getConfig filename
    str <- maybe (exitWithFailure $ "config " <> filename <> " not found") pure mStr
    maybe (exitWithFailure $ "could not decode " <> filename) pure $ Aeson.decodeStrict str

exitWithFailure :: MonadIO m => Text -> m a
exitWithFailure msg = liftIO $ do
  putStrLn msg
  exitWith $ ExitFailure 1
