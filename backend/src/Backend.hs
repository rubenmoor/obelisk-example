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
import           Common                          (RoutesApi, RouteShow, RouteMedia)
import           Common.Auth                     (UserInfo)
import           Config                          (Params (..))
import           Control.Applicative             (Applicative (pure))
import           Control.Monad.Logger            (NoLoggingT (..))
import           Control.Monad.Reader            (ReaderT (runReaderT))
import           Control.Monad.Trans             (MonadIO (liftIO))
import           Control.Monad.Trans.Resource    (runResourceT)
import           Crypto.JWT                      (JWK)
import           Data.Aeson                      (FromJSON)
import qualified Data.Aeson                      as Aeson
import           Data.Function                   (($))
import           Data.Maybe                      (maybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Data.Text.IO                    (putStrLn)
import           Database.MySQL.Base.Types       (Option (CharsetName))
import           Database.Persist.MySQL          (ConnectInfo (..),
                                                  defaultConnectInfo,
                                                  runMigration, runSqlPool,
                                                  withMySQLPool)
import           Handlers                        (handleFeedXML, handlers, mkContext)
import           Model                           (migrateAll)
import           Obelisk.Backend                 (Backend (..))
import           Obelisk.Configs                 (ConfigsT,
                                                  HasConfigs (getConfig),
                                                  runConfigsT)
import           Obelisk.ExecutableConfig.Lookup (getConfigs)
import           Obelisk.Route                   (pattern (:/))
import           Servant.Server                  (serveSnap, Context, serveSnapWithContext)
import           Snap.Core                       (Snap)
import           System.Exit                     (ExitCode (ExitFailure),
                                                  exitWith)
import           System.IO                       (IO)
import Data.Proxy (Proxy(Proxy))
import Servant (serveDirectory)

serveApi :: Context '[Snap UserInfo] -> EnvApplication -> Snap ()
serveApi ctx = runReaderT $ serveSnapWithContext (Proxy :: Proxy RoutesApi) ctx handlers

serveFeed :: EnvApplication -> Snap ()
serveFeed = runReaderT $ serveSnap (Proxy :: Proxy RouteShow) handleFeedXML

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    cfgs <- getConfigs
    (Params{..}, jwk :: JWK) <- runConfigsT cfgs $ do
      params <- getConfigOrExit "backend/params"
      jwk <- getConfigOrExit "backend/jwk"
      pure (params, jwk)
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
            , envUrl       = paramUrl
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
  where
    exitWithFailure msg = liftIO $ do
      putStrLn msg
      exitWith $ ExitFailure 1
