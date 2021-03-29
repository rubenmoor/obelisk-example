{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Auth where

import           Common.Auth             (CompactJWT (CompactJWT))
import           Control.Applicative     (Applicative (pure))
import           Control.Lens            ((?~), (^.))
import           Control.Monad.Except    (ExceptT, throwError)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Crypto.JWT              (StringOrURI, Audience (..), ClaimsSet, JWK,
                                          JWTError (..), NumericDate (..),
                                          bestJWSAlg, claimAud, claimExp,
                                          claimIat, claimSub, decodeCompact,
                                          defaultJWTValidationSettings,
                                          emptyClaimsSet, encodeCompact,
                                          newJWSHeader, signClaims,
                                          verifyClaims)
import           Data.Bool               (Bool)
import qualified Data.ByteString.Lazy    as BL
import           Data.Data               (Proxy (Proxy))
import           Data.Eq                 (Eq ((==)))
import           Data.Function           (($), (&), (.))
import           Data.Map                (Map)
import           Data.Maybe              (Maybe (..))
import           Data.String             (fromString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Time               (UTCTime, addUTCTime)
import           GHC.Base                (seq, ($!))
import           Model                   (Rank)
import           Servant                 ((:>), AuthProtect)
import           Servant.Server          (HasContextEntry (getContextEntry),
                                          HasServer (..))
import           Servant.Server.Internal (DelayedM, addAuthCheck, withRequest)
import           Snap.Core               (Request, Snap)
import           Snap.Internal.Core      (evalSnap)
import           System.IO               (IO)
import           Text.Show               (Show (show))
import AppData (Handler)

audience :: StringOrURI
audience = "https://www.serendipity.works"

mkClaims :: UTCTime -> Text -> ClaimsSet
mkClaims now subject = emptyClaimsSet
  & claimAud ?~ Audience [audience]
  & claimExp ?~ NumericDate (addUTCTime 30 now)
  & claimIat ?~ NumericDate now
  & claimSub ?~ fromString (Text.unpack subject)

-- doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError CompactJWT)
mkCompactJWT :: JWK -> ClaimsSet -> ExceptT JWTError IO CompactJWT
mkCompactJWT jwk claims = do
  alg <- bestJWSAlg jwk
  signed <- signClaims jwk (newJWSHeader ((), alg)) claims
  pure $ CompactJWT $ Text.decodeUtf8 $ BL.toStrict $ encodeCompact signed

verifyCompactJWT :: JWK -> CompactJWT -> ExceptT JWTError IO Text
verifyCompactJWT jwk (CompactJWT str)  = do
  jwt <- decodeCompact $ BL.fromStrict $ Text.encodeUtf8 str
  let config = defaultJWTValidationSettings (== audience)
  claims <- verifyClaims config jwk jwt
  case claims ^. claimSub of
    Nothing  -> throwError $ JWTClaimsSetDecodeError "no subject in claims"
    Just sub -> pure $ Text.pack $ show sub

-- servant general auth
