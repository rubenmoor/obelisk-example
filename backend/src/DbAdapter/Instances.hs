{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DbAdapter.Instances where

import Model (Rank)
import Database.Persist.TH (derivePersistFieldJSON)

derivePersistFieldJSON "Rank"
