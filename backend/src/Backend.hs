{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module Backend where

import           Route                           (BackendRoute (BackendRoute_Missing, BackendRoutes),
                                                  FrontendRoute,
                                                  fullRouteEncoder)

import           AppData                         (EnvApplication (..))
import           Common                          (routes)
import           Control.Applicative             (Applicative (pure))
import           Control.Monad                   (Monad ((>>=)))
import           Control.Monad.Reader            (ReaderT (runReaderT))
-- import           Control.Monad.Trans             (MonadIO (liftIO))
import           Crypto.JOSE                     (JWKSet (JWKSet))
import           Crypto.JWT                      (Crv (P_384), KeyMaterialGenParam (ECGenParam),
                                                  defaultJWTValidationSettings,
                                                  genJWK)
import qualified Data.Aeson                      as Aeson
import qualified Data.Aeson.Encode.Pretty        as Aeson (encodePretty)
import           Data.Bool                       (Bool (True))
import qualified Data.ByteString.Char8           as Char8
import qualified Data.ByteString.Lazy            as Lazy
import           Data.Function                   (const, ($))
import           Data.Maybe                      (maybe)
import           Data.Monoid                     ((<>))
import           Database.MySQL.Base.Types       (Option (CharsetName))
import           Database.Persist.MySQL          (ConnectInfo (..),
                                                  defaultConnectInfo,
                                                  runMigration, runSqlPool,
                                                  withMySQLPool)
import           GHC.Base                        (undefined)
import           Handlers                        (handlers)
import           Obelisk.Backend                 (Backend (..))
import           Obelisk.ExecutableConfig.Lookup (getConfigs)
import           Obelisk.Route                   (pattern (:/))
-- use obelisk's static mechanism instead
-- import           Paths_backend             (getDataFileName)
import           Config                          (Params (..))
import           Control.Monad.Logger            (NoLoggingT (..))
import           Control.Monad.Trans.Resource    (runResourceT)
import           Model                           (migrateAll)
import           Obelisk.Configs                 (HasConfigs (getConfig),
                                                  runConfigsT)
import           Servant                         ((:<|>) (..), serveDirectory)
import           Servant.Server                  (serveSnap)
import           Snap.Core                       (Snap)
import           System.Directory                (doesFileExist)
import           System.Exit                     (ExitCode (ExitFailure),
                                                  exitWith)
import           System.IO                       (putStrLn)

backendApp :: EnvApplication -> Snap ()
backendApp = runReaderT $ serveSnap routes handlers

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    -- TODO: read key file from obelisk backend configuration
    -- str <- liftIO $ Lazy.readFile optKeyFileName
    -- jwk <- liftIO $ maybe (exitWith $ ExitFailure 1) pure $ Aeson.decode str
    -- TODO: read connection info from configuration file
    cfgs <- getConfigs
    mStr <- runConfigsT cfgs $ getConfig "backend/params"
    let mParams = mStr >>= Aeson.decodeStrict
    let exitWithFailure = do
          putStrLn "Could not parse configuration"
          exitWith $ ExitFailure 1
    Params{..} <- maybe exitWithFailure pure mParams
    let connectInfo = defaultConnectInfo
          { connectHost = paramDbHost
          , connectUser = paramDbUser
          , connectPassword = paramDbPassword
          , connectDatabase = paramDbDatabase
          , connectOptions = [ CharsetName "utf8mb4" ]
          }
        -- ctxJWT = JWTSettings (SomeJWKResolver $ JWKSet [jwk])
        --                      (defaultJWTValidationSettings $ const True)
        -- ctx = ctxJWT :. EmptyContext
    runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
      runResourceT $ runSqlPool (runMigration migrateAll) pool
      let env = EnvApplication
            { envPool      = pool
            , envMediaDir  = paramMediaDir
            , envUrl       = paramUrl
            }
      NoLoggingT $ serve $ \case
        (BackendRoute_Missing :/ ()) -> pure ()
        -- TODO: serve public dir via obelisk
        (BackendRoutes        :/  _) -> backendApp env
  , _backend_routeEncoder = fullRouteEncoder
  }
