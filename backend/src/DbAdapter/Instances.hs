{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module DbAdapter.Instances where

import           Control.Applicative     (Applicative (pure))
import           Control.Category        (Category ((.)))
import           Control.Monad.Fail      (MonadFail (fail))
import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Aeson              as Aeson
import           Data.Aeson.Encoding     (string)
import           Data.Aeson.Types        (unexpected)
import           Data.Either             (Either (..))
import           Data.Either.Combinators (mapLeft)
import           Data.Eq                 (Eq)
import           Data.Function           (($))
import           Data.Functor            ((<$>))
import           Data.Ord                (Ord)
import           Data.Semigroup          (Semigroup ((<>)))
import qualified Data.Text               as Text
import           Data.Text.Encoding      (encodeUtf8)
import           Data.ByteString.Lazy          (toStrict)
import           Database.Gerippe        (PersistField (..),
                                          PersistFieldSql (..),
                                          PersistValue (PersistByteString),
                                          SqlType (..))
import           Database.Persist.TH     (derivePersistFieldJSON)
import           GHC.Generics            (Generic)
import           Text.Read               (Read)
import           Text.Show               (Show (show))
import           Text.URI                (URI, mkURI, render)
import Model (Rank)

derivePersistFieldJSON "Rank"
