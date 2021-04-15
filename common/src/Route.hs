{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)

import           Control.Category      (Category (id))
import           Control.Monad         (mapM)
import           Data.Either           (Either)
import           Data.Foldable         (concat)
import           Data.Function         (($))
import           Data.Functor          ((<$>))
import           Data.Monoid           (Monoid (mempty))
import           Obelisk.Route         (pattern (:/), Encoder,
                                        FullRoute (FullRoute_Backend), PageName,
                                        R, SegmentResult (PathEnd, PathSegment),
                                        mkFullRouteEncoder, unitEncoder)
import           Obelisk.Route.TH      (deriveRouteComponent)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName
  BackendRoute_Show :: BackendRoute PageName
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main     :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Login    :: FrontendRoute ()
  FrontendRoute_AliasSelect :: FrontendRoute ()
  FrontendRoute_AliasRename :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" id
      BackendRoute_Show -> PathSegment "show" id
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_AliasSelect -> PathSegment "alias-select" $ unitEncoder mempty
      FrontendRoute_AliasRename -> PathSegment "alias-rename" $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
