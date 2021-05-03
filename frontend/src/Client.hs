{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}

module Client where

import           Common              (EpisodeNew, RoutesApi)
import           Common.Auth         (CompactJWT, LoginData (..),
                                      SessionData (..), UserNew (..))
import           Control.Applicative (Applicative (pure), (<$>))
import           Control.Monad       (Monad)
import           Data.Bool           (Bool)
import           Data.Data           (Proxy (..))
import           Data.Either         (Either (..))
import           Data.Function       (($))
import           Data.Functor        (Functor (fmap))
import           Data.Maybe          (Maybe)
import           Data.Text           (Text)
import           Model               (Episode, Platform, Podcast)
import           Reflex.Dom          (Prerender (Client, prerender),
                                      Reflex (Dynamic, Event, never), constDyn,
                                      ffor, switchDyn)
import           Servant.API         ((:<|>) (..))
import           Servant.Reflex      (BaseUrl (..), ReqResult,
                                      SupportsServantReflex, client)
import           State               (Session (..), State (stSession))

request
  :: (Prerender js t m, Monad m)
  => Client m (Event t (ReqResult () a))
  -> m (Event t (ReqResult () a))
request r = switchDyn <$> prerender (pure never) r

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

-- episode

postEpisodeNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text) -- podcast identifier
  -> Dynamic t (Either Text (CompactJWT, Text))
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
  -> m (Event t (ReqResult () ()))

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

(
       (    postAuthenticate
       :<|> postAuthNew
       :<|> postDoesUserExist
       )
  :<|> (postEpisodeNew)
  :<|> (    postPodcastNew
       :<|> getPodcast
       )
  :<|> (    postAliasRename
       :<|> getAliasAll
       :<|> postAliasSetDefault
       )
  ) =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))
