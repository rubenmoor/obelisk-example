{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import           Route                           (BackendRoute(BackendRoute_Show, BackendRoute_Missing, BackendRoute_Api),
                                                  FrontendRoute,
                                                  fullRouteEncoder)

import           AppData                         (EnvApplication (..))
import           Common                          (routes)
import           Control.Applicative             (Applicative (pure))
import           Control.Monad                   (Monad ((>>=)))
import           Control.Monad.Reader            (ReaderT (runReaderT))
import           Control.Monad.Trans             (MonadIO (liftIO))
import           Crypto.JOSE                     (JWKSet (JWKSet))
import           Crypto.JWT                      (Crv (P_384), JWK, KeyMaterialGenParam (ECGenParam),
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
import           Data.Aeson                      (FromJSON)
import           Data.Text                       (Text)
import           Data.Text.IO                    (putStrLn)
import           Model                           (migrateAll)
import           Obelisk.Configs                 (ConfigsT,
                                                  HasConfigs (getConfig),
                                                  runConfigsT)
import           Servant                         ((:<|>) (..), serveDirectory)
import           Servant.Server                  (serveSnap)
import           Snap.Core                       (Snap)
import           System.Directory                (doesFileExist)
import           System.Exit                     (ExitCode (ExitFailure),
                                                  exitWith)
import           System.IO                       (IO)

backendApp :: EnvApplication -> Snap ()
backendApp = runReaderT $ serveSnap routes handlers

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
        (BackendRoute_Api     :/  _) -> backendApp env
        (BackendRoute_Show    :/  _) -> backendApp env
  , _backend_routeEncoder = fullRouteEncoder
  }

getConfigOrExit :: forall a. FromJSON a => Text -> ConfigsT IO a
getConfigOrExit filename = do
  mStr <- getConfig filename
  str <- maybe (exitWithFailure $ "config " <> filename <> " not found") pure mStr
  maybe (exitWithFailure $ "could not decode " <> filename) pure $ Aeson.decodeStrict str
  where
    exitWithFailure msg = liftIO $ do
      putStrLn msg -- "Could not parse configuration"
      exitWith $ ExitFailure 1
