{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}

module Client where

import           Common.Api          (EpisodeNew, RoutesApi)
import           Common.Auth         (CompactJWT, LoginData (..),
                                      SessionData (..), UserNew (..))
import           Control.Applicative (Applicative (pure))
import           Control.Monad       (Monad)
import           Data.Bool           (Bool)
import           Data.Data           (Proxy (..))
import           Data.Either         (either, Either (..))
import           Data.Function       (const, ($))
import Data.Functor (Functor)
import           Data.Maybe          (fromMaybe, Maybe (..))
import           Data.Text           (Text)
import           Common.Model               (Episode, Platform, Podcast)
import           Reflex.Dom          (xhrResponse_response, XhrResponseBody (..), Prerender (Client, prerender),
                                      Reflex (Dynamic, Event, never), constDyn,
                                      ffor, switchDyn)
import           Servant.API         ((:<|>) (..))
import           Servant.Reflex      (BaseUrl (..), ReqResult,
                                      SupportsServantReflex, client)
import           State               (Session (..), State (stSession))
import Servant.Common.Req (ReqResult(..))
import Data.Functor (Functor(fmap))
import Control.Category ((<<<))
import Control.Lens.Getter ((^.))
import Data.Text.Encoding (decodeUtf8')
import Data.Semigroup (Semigroup((<>)))

postRender :: (Prerender t m, Monad m) => Client m (Event t a) -> m (Event t a)
postRender = fmap switchDyn <<< prerender (pure never)

request
    :: forall t (m :: * -> *) a
     . (Monad m, Prerender t m)
    => Client m (Event t (ReqResult () a))
    -> m (Event t (Either Text a))
request =
    fmap (fmap resultToEither <<< switchDyn) <<< prerender (pure never)

resultToEither :: ReqResult () a -> Either Text a
resultToEither = \case
    ResponseSuccess _ x _ -> Right x
    ResponseFailure _ _ resp ->
      let mErrBody = do
              responseBody <- resp ^. xhrResponse_response
              bs           <- case responseBody of
                  XhrResponseBody_ArrayBuffer bs -> pure bs
                  _                              -> Nothing
              either (const Nothing) pure $ decodeUtf8' bs
      in  Left $ fromMaybe "Couldn't decode error body" mErrBody
    RequestFailure _ str -> Left $ "RequestFailure: " <> str

getAuthData
  :: Functor (Dynamic t)
  => Dynamic t State
  -> Dynamic t (Either Text (CompactJWT, Text))
getAuthData dynState =
  ffor dynState $ \st -> case stSession st of
    SessionAnon                 -> Left "not logged in"
    SessionUser SessionData{..} -> Right (sdJwt, sdAliasName)

postAuthenticate
  :: SupportsServantReflex t m
  => Dynamic t (Either Text LoginData)
  -> Event t ()
  -> m (Event t (ReqResult () (Maybe SessionData)))

postAuthNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text UserNew)
  -> Event t ()
  -> m (Event t (ReqResult () SessionData))

postDoesUserExist
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () Bool))

postDoesAliasExist
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () Bool))

postLogout
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Event t ()
  -> m (Event t (ReqResult () ()))

-- episode

postEpisodeNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Dynamic t (Either Text Text) -- podcast identifier
  -> Dynamic t (Either Text EpisodeNew)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

-- podcast

postPodcastNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Dynamic t (Either Text Text) -- podcast identifier
  -> Event t ()
  -> m (Event t (ReqResult () ()))

getPodcast
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text) -- podcast identifier
  -> Event t ()
  -> m (Event t (ReqResult () (Podcast, [Platform], [Episode])))

-- user

postAliasRename
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () Text))

getAliasAll
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Event t ()
  -> m (Event t (ReqResult () [Text]))

postAliasSetDefault
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

postAliasVisibility
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (CompactJWT, Text))
  -> Dynamic t (Either Text Bool)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

(
       (    postAuthenticate
       :<|> postAuthNew
       :<|> postDoesUserExist
       :<|> postDoesAliasExist
       :<|> postLogout
       )
  :<|> (    postEpisodeNew
       :<|> postPodcastNew
       :<|> getPodcast
       )
  :<|> (    postAliasRename
       :<|> getAliasAll
       :<|> postAliasSetDefault
       :<|> postAliasVisibility
       )
  ) =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))
