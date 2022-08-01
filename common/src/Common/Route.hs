{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric         #-}
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

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import           Data.Functor.Identity (Identity)
import           Data.Text             (Text)

import           Control.Category      (Category (id, (.)))
import           Control.Lens          (Wrapped)
import           Control.Monad         (mapM)
import           Data.Either           (Either)
import           Data.Foldable         (concat)
import           Data.Function         (($))
import           Data.Functor          ((<$>))
import           Data.Monoid           (mempty)
import           GHC.Generics          (Generic)
import           Obelisk.Route         (singletonListEncoder, pathParamEncoder, maybeEncoder, pathSegmentEncoder, pattern (:/), Encoder,
                                        FullRoute (FullRoute_Backend), PageName,
                                        R, SegmentResult (PathEnd, PathSegment),
                                        mkFullRouteEncoder,
                                        singlePathSegmentEncoder, unitEncoder,
                                        unwrappedEncoder)
import           Obelisk.Route.TH      (deriveRouteComponent)
import Data.Maybe (Maybe)
import Text.Show (Show)

newtype PodcastIdentifier = PodcastIdentifier
  { unPodcastIdentifier :: Text }
  deriving (Generic)

instance Wrapped PodcastIdentifier

newtype EpisodeSlug = EpisodeSlug
  { unEpisodeSlug :: Text }
  deriving (Generic, Show)

instance Wrapped EpisodeSlug

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Show :: BackendRoute PageName
  BackendRoute_Media :: BackendRoute PageName
  BackendRoute_Api :: BackendRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Main     :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Login    :: FrontendRoute ()
  FrontendRoute_AliasSelect :: FrontendRoute ()
  FrontendRoute_AliasRename :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Podcast :: FrontendRoute (PodcastIdentifier, Maybe EpisodeSlug)

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Show -> PathSegment "show" id
      BackendRoute_Media -> PathSegment "media" id
      BackendRoute_Api -> PathSegment "api" id
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_AliasSelect -> PathSegment "alias-select" $ unitEncoder mempty
      FrontendRoute_AliasRename -> PathSegment "alias-rename" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_Podcast -> PathSegment "presents" $
        pathParamEncoder unwrappedEncoder $
            maybeEncoder (unitEncoder mempty) $
                singlePathSegmentEncoder . unwrappedEncoder
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
