{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Client where

import           Common         (EpisodeNew, RoutesApi)
import           Common.Auth    (CompactJWT, LoginData (..), UserNew (..), UserInfo)
import           Data.Bool      (Bool)
import           Data.Data      (Proxy (..))
import           Data.Either    (Either)
import           Data.Maybe     (Maybe)
import           Data.Text      (Text)
import           Reflex.Dom     (switchDyn, Prerender(Client, prerender),  Reflex(never, Dynamic, Event), constDyn)
import           Servant.API    ((:<|>) (..))
import           Servant.Reflex (BaseUrl (..), ReqResult, SupportsServantReflex,
                                 client)
import Control.Applicative (Applicative(pure), (<$>))
import Control.Monad (Monad)

request
  :: (Prerender js t m, Monad m)
  => Client m (Event t (ReqResult () a))
  -> m (Event t (ReqResult () a))
request r = switchDyn <$> prerender (pure never) r

postAuthenticate
  :: SupportsServantReflex t m
  => Dynamic t (Either Text LoginData)
  -> Event t ()
  -> m (Event t (ReqResult () (Maybe (CompactJWT, UserInfo))))

postAuthNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text UserNew)
  -> Event t ()
  -> m (Event t (ReqResult () (CompactJWT, UserInfo)))

postDoesUserExist
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () Bool))

postEpisodeNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text) -- podcast identifier
  -> Dynamic t (Either Text CompactJWT)
  -> Dynamic t (Either Text Text) -- alias
  -> Dynamic t (Either Text EpisodeNew)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

postAliasRename
  :: SupportsServantReflex t m
  => Dynamic t (Either Text CompactJWT)
  -> Dynamic t (Either Text Text) -- alias
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

getAliasAll
  :: SupportsServantReflex t m
  => Dynamic t (Either Text CompactJWT)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () [Text]))

postAliasSetDefault
  :: SupportsServantReflex t m
  => Dynamic t (Either Text CompactJWT)
  -> Dynamic t (Either Text Text) -- alias
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

(
       (    postAuthenticate
       :<|> postAuthNew
       :<|> postDoesUserExist
       )
  :<|> (postEpisodeNew)
  :<|> (    postAliasRename
       :<|> getAliasAll
       :<|> postAliasSetDefault
       )
  ) =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))
