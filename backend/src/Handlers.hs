module Handlers
  ( handlers
  ) where

import qualified Data.ByteString.Lazy as Lazy
import AppData (Handler)

handlers = handleFeedXML

handleFeedXML :: Handler Lazy.ByteString
handleFeedXML = undefined
