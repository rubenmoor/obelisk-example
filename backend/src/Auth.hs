{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Auth
  ( module Auth
  , module Common.Auth
  ) where

import           Common.Auth          (AuthProtect,
                                       CompactJWT (CompactJWT),
                                       SessionData (..))
import           Control.Applicative  (Applicative (pure))
import           Control.Category     (Category ((.)))
import           Control.Lens         ((?~), (^.), (^?), _Just)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Time   (MonadTime)
import           Crypto.JWT           (Audience (..), ClaimsSet, JWK,
                                       JWTError (..), MonadRandom,
                                       NumericDate (..), StringOrURI,
                                       bestJWSAlg, claimAud, claimExp, claimIat,
                                       claimSub, decodeCompact,
                                       defaultJWTValidationSettings,
                                       emptyClaimsSet, encodeCompact,
                                       newJWSHeader, signClaims, string,
                                       verifyClaims)
import           Data.Bool            (Bool (..))
import qualified Data.ByteString.Lazy as BL
import           Data.Eq              (Eq ((==)))
import           Data.Function        (($), (&))
import           Data.Map             (Map)
import           Data.Maybe           (Maybe (..))
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time            (UTCTime, addUTCTime)
import           Database.Gerippe     (Key)
import           DbAdapter            (Alias (..))
import           GHC.Generics         (Generic)
import           Model                (Rank)
import Servant.Server (HasContextEntry(getContextEntry),  HasServer (..))
import Snap.Core (Request, Snap)
import Servant.API ((:>))
import Data.Proxy (Proxy(Proxy))
import Servant.Server.Internal (withRequest, addAuthCheck, DelayedM)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Snap.Internal.Core (evalSnap)
import GHC.Base (($!))
import GHC.Exts (seq)
import GHC.TypeLits (KnownSymbol)

audience :: StringOrURI
audience = "https://www.serendipity.works"

mkClaims :: UTCTime -> Text -> ClaimsSet
mkClaims now sub = emptyClaimsSet
  & claimAud ?~ Audience [audience]
  & claimExp ?~ NumericDate (addUTCTime 30 now)
  & claimIat ?~ NumericDate now
  & claimSub ?~ fromString (Text.unpack sub)

mkCompactJWT
  :: (MonadRandom m, MonadError JWTError m)
  => JWK
  -> ClaimsSet
  -> m CompactJWT
mkCompactJWT jwk claims = do
  alg <- bestJWSAlg jwk
  signed <- signClaims jwk (newJWSHeader ((), alg)) claims
  pure $ CompactJWT $ Text.decodeUtf8 $ BL.toStrict $ encodeCompact signed

verifyCompactJWT
  :: (MonadError JWTError m, MonadTime m)
  => JWK
  -> CompactJWT
  -> m Text
verifyCompactJWT jwk (CompactJWT str)  = do
  jwt <- decodeCompact $ BL.fromStrict $ Text.encodeUtf8 str
  let config = defaultJWTValidationSettings (== audience)
  claims <- verifyClaims config jwk jwt
  case claims ^. claimSub ^? _Just . string of
    Nothing -> throwError $ JWTClaimsSetDecodeError "no subject in claims"
    Just s  -> pure s

data UserInfo = UserInfo
  { uiIsSiteAdmin :: Bool
  , uiUserName    :: Text
  , uiAlias       :: Alias
  , uiKeyAlias    :: Key Alias
  , uiClearances  :: Map Text Rank
  } deriving (Generic)

mkSessionData :: CompactJWT -> UserInfo -> SessionData
mkSessionData jwt UserInfo{..} =
  let sdJwt = jwt
      sdIsSiteAdmin = uiIsSiteAdmin
      sdUserName = uiUserName
      sdAliasName = aliasName uiAlias
      sdClearances = uiClearances
  in  SessionData{..}

instance ( KnownSymbol tag
         , HasServer api context m
         , HasContextEntry context (Snap UserInfo)
         )
  => HasServer (AuthProtect tag :> api) context m where

  type ServerT (AuthProtect tag :> api) context m =
    UserInfo -> ServerT api context m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route (Proxy :: Proxy (AuthProtect tag :> api)) context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` withRequest authCheck)
      where
        authCheck :: Request -> DelayedM m UserInfo
        authCheck =
          liftIO . evalSnap (getContextEntry context)
                            (\x -> pure $! (x `seq` ()))
                            (\f -> let !_ = f 0 in pure ())
