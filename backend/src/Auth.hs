{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Auth where

import           Common.Auth          (CompactJWT (CompactJWT))
import           Control.Applicative  (Applicative (pure))
import           Control.Lens         ((?~), (^.))
import           Control.Monad.Except (ExceptT, throwError)
import           Control.Monad.Trans  (MonadIO)
import           Crypto.JWT           (Audience (..), ClaimsSet, JWK,
                                       JWTError (..), MonadRandom,
                                       NumericDate (..), bestJWSAlg, claimAud,
                                       claimExp, claimIat, claimIss, claimSub,
                                       decodeCompact,
                                       defaultJWTValidationSettings,
                                       emptyClaimsSet, encodeCompact,
                                       newJWSHeader, signClaims, verifyClaims)
import qualified Data.ByteString.Lazy as BL
import           Data.Eq              (Eq ((==)))
import           Data.Function        (($), (&), (.))
import           Data.Maybe           (Maybe (..), maybe)
import           Data.String          (fromString)
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import           Data.Time            (UTCTime, addUTCTime)
import           System.IO            (IO)
import           Text.Show            (Show (show))

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
