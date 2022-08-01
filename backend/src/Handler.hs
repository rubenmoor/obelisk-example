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

module Handler
  ( handlers
  , handleFeedXML
  ) where

import           AppData                   (EnvApplication (..),
                                            Handler)
import           Auth                      (UserInfo (..), mkClaims,
                                            mkCompactJWT, verifyCompactJWT)
import Auth ()
import           Common.Api                (EpisodeNew (..), RoutesApi)
import           Common                (convertToFilename, formatDuration)
import           Common.Auth               (LoginData (..), SessionData (..),
                                            UserNew (..))
import           Control.Applicative       (Applicative (pure))
import           Control.Category          (Category ((.)))
import           Control.Monad             (Monad ((>>=)), unless, when)
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.IO.Class    (MonadIO (liftIO))
import           Control.Monad.Reader      (asks)
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
                                            PersistUniqueRead (getBy), from, get,
                                            getAll, getWhere, joinMTo1Where',
                                            on, select, val, where_, (&&.),
                                            (==.), (^.))
import           Database.Persist.MySQL    (PersistStoreWrite (update),
                                            SqlBackend, (=.))
import           DbAdapter                 (Alias (..), AuthPwd (..),
                                            Clearance (..), EntityField (..),
                                            Unique (..),
                                            User (..))
import qualified DbAdapter                 as Db
import           Common.Model                     (Episode (..),
                                            Journal (..), Platform (..),
                                            Podcast (..), Rank (RankModerator),
                                            Visibility (..), JournalEvent (..), EventUser (..), EventApp (..))
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

import Database (runDb, runDb')
import qualified DbJournal
import qualified Handler.Auth as Auth
import qualified Handler.User as User
import qualified Handler.Serendipity as Serendipity
import Handler.Feed (handleFeedXML)

default(Text)

handlers :: ServerT RoutesApi '[Snap UserInfo] Handler
handlers =
       Auth.handlers
  :<|> Serendipity.handlers
  :<|> User.handlers
