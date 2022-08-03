{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbAdapter.Instances where

import Common.Model (Rank, Visibility)
import Database.Persist.TH (derivePersistFieldJSON)

derivePersistFieldJSON "Rank"
derivePersistFieldJSON "Visibility"
