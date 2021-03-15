{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Backend where

import Common.Route
    ( FrontendRoute, BackendRoute(BackendRoute_Missing),  fullRouteEncoder )
import Obelisk.Backend ( Backend(..) )
import Data.Function ( ($), const )
import Control.Applicative (Applicative(pure))
import Obelisk.Route (pattern (:/))

backendApp :: EnvApplication -> Application
backendApp env@EnvApplication{..} =
  serveSnap routes (handlers :<|> serveDirectoryWebApp envPublicDir)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      serve $ \case
        (BackendRoute_Missing :/ ()) -> pure ()
        (BackendRoutes        :/  _) -> backendApp
  , _backend_routeEncoder = fullRouteEncoder
  }
