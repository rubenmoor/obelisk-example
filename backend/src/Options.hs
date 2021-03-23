{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Options where

import           Control.Applicative ((<$>), (<*>))
import           Data.Bool           (Bool)
import           Data.ByteString     (ByteString)
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import           Network.Socket      (HostName)
import           Options.Applicative (Parser, help, long, metavar, showDefault,
                                      strOption, switch, value)
import           System.IO           (FilePath)

data Options = Options
    { optUrl             :: Text
    , optPublicDir       :: FilePath
    , optMediaDir        :: FilePath
    , optHost            :: HostName
    , optUser            :: ByteString
    , optPwd             :: ByteString
    , optDbName          :: ByteString
    , optKeyFileName     :: FilePath
    , optGenerateKey     :: Bool
    }

parseMyOptions :: Parser Options
parseMyOptions = Options
  <$> strOption (
           long "url"
        <> metavar "URL"
        <> value "http://localhost:3000/"
        <> help "the url of the homepage"
        <> showDefault
        )
  <*> strOption (
           long "public-directory"
        <> metavar "PUBLICDIR"
        <> value "backend/static"
        <> help "filesystem directory for static files: *.css, *.js"
        <> showDefault
        )
  <*> strOption (
            long "media-directory"
        <> metavar "MEDIADIR"
        <> value "backend/static/media"
        <> help "audio files will be copied here"
        <> showDefault
        )
  <*> strOption (
           long "mysql-host"
        <> metavar "HOST"
        <> value "localhost"
        <> help "mysql host"
        <> showDefault
        )
  <*> strOption (
           long "mysql-user"
        <> metavar "USER"
        <> value "podcast"
        <> help "mysql user"
        <> showDefault
        )
  <*> strOption (
           long "mysql-password"
        <> metavar "PASSWORD"
        <> value "password"
        <> help "mysql password for given user"
        <> showDefault
        )
  <*> strOption (
           long "mysql-database"
        <> metavar "DATABASE_NAME"
        <> value "podcast"
        <> help "mysql database name"
        <> showDefault
        )
  <*> strOption (
           long "key-filename"
        <> help "path to key file for JWT"
        <> metavar "KEYFILENAME"
        <> value "key.json"
        <> showDefault
        )
  <*> switch (
           long "generate-key"
        <> help "generate a key pair and exit"
        <> showDefault
        )
