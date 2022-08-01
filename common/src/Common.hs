{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Common where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            (($))
import           Data.Int                 (Int)
import           Data.Text                (Text, replace)
import qualified Data.Text                as Text
import           GHC.Num                  (Num ((*)))
import           GHC.Real                 (Integral (div, mod))
import           Text.Printf              (printf)

convertToFilename :: Text -> Text
convertToFilename str =
  let map = [ ("Ä", "A")
            , ("Ö", "O")
            , ("Ü", "U")
            , ("ß", "SS")
            , ("?", "_")
            , ("!", "_")
            , (",", "_")
            , (";", "_")
            , (":", "_")
            , ("'", "_")
            , ("=", "_")
            , ("<", "_")
            , (">", "_")
            , ("/", "_")
            , ("\\", "_")
            , ("\"", "_")
            , ("&", "_")
            , ("@", "_")
            , ("%", "_")
            , ("+", "_")
            , ("*", "_")
            , ("$", "_")
            , (" ", "_")
            , ("(", "_")
            , (")", "_")
            , ("É", "E")
            , ("Á", "A")
            , ("Í", "I")
            , ("È", "E")
            , ("À", "A")
            , ("Ì", "I")
            ]
      acc str' (s, t) = replace s t str'
  in  foldl' acc str map

formatDuration :: Int -> Text
formatDuration d =
  let seconds = d `mod` 60
      minutes = (d `div` 60) `mod` 60
      hours = d `div` (60 * 60)
  in  Text.pack $ printf "%02d:%02d:%02d" hours minutes seconds
