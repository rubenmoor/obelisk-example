{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Auth where

import           Common.Auth          (CompactJWT (CompactJWT))
import           Control.Applicative  (Applicative (pure))
import           Control.Lens         ((?~), (^.))
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.Time   (MonadTime)
import           Crypto.JWT           (Audience (..), ClaimsSet, JWK,
                                       JWTError (..), MonadRandom,
                                       NumericDate (..), StringOrURI,
                                       bestJWSAlg, claimAud, claimExp, claimIat,
                                       claimSub, decodeCompact,
                                       defaultJWTValidationSettings,
                                       emptyClaimsSet, encodeCompact,
                                       newJWSHeader, signClaims, verifyClaims)
import qualified Data.ByteString.Lazy as BL
import           Data.Eq              (Eq ((==)))
import           Data.Function        (($), (&))
import           Data.Maybe           (Maybe (..))
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time            (UTCTime, addUTCTime)
import           Text.Show            (Show (show))

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
  => JWK -> ClaimsSet -> m CompactJWT
mkCompactJWT jwk claims = do
  alg <- bestJWSAlg jwk
  signed <- signClaims jwk (newJWSHeader ((), alg)) claims
  pure $ CompactJWT $ Text.decodeUtf8 $ BL.toStrict $ encodeCompact signed

verifyCompactJWT
  :: (MonadError JWTError m, MonadTime m)
  => JWK -> CompactJWT -> m Text
verifyCompactJWT jwk (CompactJWT str)  = do
  jwt <- decodeCompact $ BL.fromStrict $ Text.encodeUtf8 str
  let config = defaultJWTValidationSettings (== audience)
  claims <- verifyClaims config jwk jwt
  case claims ^. claimSub of
    Nothing  -> throwError $ JWTClaimsSetDecodeError "no subject in claims"
    Just s   -> pure $ Text.pack $ show s
