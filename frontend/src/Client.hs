{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Client where

import           Auth           (CompactJWT, Credentials (..), RespLogin)
import           Common         (EpisodeNew, RoutesApi)
import           Data.Bool      (Bool)
import           Data.Data      (Proxy (..))
import           Data.Either    (Either)
import           Data.Maybe     (Maybe)
import           Data.Text      (Text)
import           Reflex.Dom     (Reflex (Dynamic, Event), constDyn)
import           Servant.API    ((:<|>) (..))
import           Servant.Reflex (BaseUrl (..), ReqResult, SupportsServantReflex,
                                 client)

postAuthenticate
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Credentials)
  -> Event t ()
  -> m (Event t (ReqResult () RespLogin))

postAuthNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Credentials)
  -> Event t ()
  -> m (Event t (ReqResult () (Maybe CompactJWT)))

postUserExists
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () Bool))

postEpisodeNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text)
  -> Dynamic t (Either Text EpisodeNew)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

((postAuthenticate :<|> postAuthNew :<|> postUserExists) :<|> postEpisodeNew) =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))
