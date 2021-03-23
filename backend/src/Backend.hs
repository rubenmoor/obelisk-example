{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE OverloadedStrings #-}

module Backend where

import           Route                     (BackendRoute (BackendRoute_Missing, BackendRoutes),
                                            FrontendRoute, fullRouteEncoder)

import           AppData                   (EnvApplication (..))
import           Common                    (routes)
import           Control.Applicative       (Applicative (pure))
import           Control.Monad             (Monad ((>>=)))
import           Control.Monad.Reader      (ReaderT (runReaderT))
import           Control.Monad.Trans       (MonadIO (liftIO))
import           Crypto.JOSE               (JWKSet (JWKSet))
import           Crypto.JWT                (Crv (P_384),
                                            KeyMaterialGenParam (ECGenParam),
                                            defaultJWTValidationSettings,
                                            genJWK)
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Encode.Pretty  as Aeson (encodePretty)
import           Data.Bool                 (Bool (True))
import qualified Data.ByteString.Char8     as Char8
import qualified Data.ByteString.Lazy      as Lazy
import           Data.Function             (const, ($))
import           Data.Maybe                (maybe)
import           Data.Monoid               ((<>))
import           Database.MySQL.Base.Types (Option (CharsetName))
import           Database.Persist.MySQL    (ConnectInfo (..),
                                            defaultConnectInfo)
import           GHC.Base                  (undefined)
import           Handlers                  (handlers)
import           Obelisk.Backend           (Backend (..))
import           Obelisk.Route             (pattern (:/))
-- use obelisk's static mechanism instead
-- import           Paths_backend             (getDataFileName)
import           Servant                   ((:<|>) (..), serveDirectory)
import           Servant.Server            (serveSnap)
import           System.Directory          (doesFileExist)
import           System.Exit               (ExitCode (ExitFailure), exitWith)
import           System.IO                 (putStrLn)
import Snap.Core (Snap)

backendApp :: EnvApplication -> Snap ()
backendApp = runReaderT $ serveSnap routes handlers

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- TODO: move to different executable/subproject
      -- if optGenerateKey
      --   then liftIO $ do
      --     putStrLn $ "Generating key " <> optKeyFileName <> "."
      --     exists <- doesFileExist optKeyFileName
      --     if exists
      --       then do putStrLn "File exists. Exiting."
      --               exitWith $ ExitFailure 1
      --       else do jwk <- genJWK (ECGenParam P_384)
      --               Lazy.writeFile optKeyFileName (Aeson.encodePretty jwk)
          -- TODO: read key file from obelisk backend configuration
          -- str <- liftIO $ Lazy.readFile optKeyFileName
          -- jwk <- liftIO $ maybe (exitWith $ ExitFailure 1) pure $ Aeson.decode str
          -- TODO: read connection info from configuration file
          let connectInfo = defaultConnectInfo
                { connectHost = "localhost"
                , connectUser = Char8.unpack "podcast"
                , connectPassword = Char8.unpack "podcast"
                , connectDatabase = Char8.unpack "podcast"
                , connectOptions = [ CharsetName "utf8mb4" ]
                }
              -- ctxJWT = JWTSettings (SomeJWKResolver $ JWKSet [jwk])
              --                      (defaultJWTValidationSettings $ const True)
              -- ctx = ctxJWT :. EmptyContext
          -- tbd. createPool
          -- runNoLoggingT $ withMySQLPool connectInfo 10 $ \pool -> do
          -- runResourceT $ runSqlPool (runMigration migrateAll) pool
          let env = EnvApplication
                { envPool      = undefined -- pool
                -- TODO: directory management via backend configuration file
                , envMediaDir  = ""
                , envUrl       = "http://localhost:8000"
                , envPublicDir = ""
                }
          serve $ \case
            (BackendRoute_Missing :/ ()) -> pure ()
            -- TODO: serve public dir via obelisk
            (BackendRoutes        :/  _) -> backendApp env
  , _backend_routeEncoder = fullRouteEncoder
  }
