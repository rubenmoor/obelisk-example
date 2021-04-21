{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module DbAdapter.Instances where

import Model (Rank)
import Database.Persist.TH (derivePersistFieldJSON)

derivePersistFieldJSON "Rank"
