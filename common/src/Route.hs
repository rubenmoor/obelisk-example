{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
module Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity ( Identity )

import Obelisk.Route
    ( Encoder,
      R,
      FullRoute(FullRoute_Backend),
      PageName,
      SegmentResult(PathEnd, PathSegment),
      mkFullRouteEncoder,
      pattern (:/),
      unitEncoder )
import Obelisk.Route.TH ( deriveRouteComponent )
import Data.Either (Either)
import Data.Functor ((<$>))
import Control.Monad (mapM)
import Data.Foldable (concat)
import Control.Category (Category(id))
import Data.Function (($))
import Data.Monoid (Monoid(mempty))

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoutes :: BackendRoute PageName
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoutes -> PathSegment "routes" id
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
