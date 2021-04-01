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
import           Reflex.Dom     (Reflex (Dynamic, Event), constDyn)
import           Servant.API    ((:<|>) (..))
import           Servant.Reflex (BaseUrl (..), ReqResult, SupportsServantReflex,
                                 client)

postAuthenticate
  :: SupportsServantReflex t m
  => Dynamic t (Either Text LoginData)
  -> Event t ()
  -> m (Event t (ReqResult () (Maybe CompactJWT)))

postAuthNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text UserNew)
  -> Event t ()
  -> m (Event t (ReqResult () CompactJWT))

postDoesUserExist
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () Bool))

postAuthUserGet
  :: SupportsServantReflex t m
  => Dynamic t (Either Text CompactJWT) -- compactJWT
  -> Event t ()
  -> m (Event t (ReqResult () UserInfo))

postEpisodeNew
  :: SupportsServantReflex t m
  => Dynamic t (Either Text Text) -- podcast identifier
  -> Dynamic t (Either Text CompactJWT) -- compactJWT
  -> Dynamic t (Either Text EpisodeNew)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

postAliasRename
  :: SupportsServantReflex t m
  => Dynamic t (Either Text CompactJWT)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () ()))

((postAuthenticate :<|> postAuthNew :<|> postDoesUserExist :<|> postAuthUserGet)
   :<|> (postEpisodeNew)
   :<|> postAliasRename
 ) =
  client (Proxy :: Proxy RoutesApi)
         (Proxy :: Proxy (m :: * -> *))
         (Proxy :: Proxy ())
         (constDyn (BasePath "/"))
