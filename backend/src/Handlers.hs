{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards     #-}

module Handlers
  ( handlers
  ) where

import           AppData              (Handler)
import           Auth                 (CompactJWT, Credentials (..),
                                       LoginFailure (..), RespLogin (..))
import           Common               (Routes, EpisodeNew)
import qualified Data.ByteString.Lazy as Lazy
import           Data.Text            (Text)
import           Servant.API          ((:<|>) (..))
import           Servant.Server       (HasServer (ServerT))

handlers :: ServerT Routes '[] Handler
handlers =
    handleFeedXML
  :<|>  (handleGrantAuthPwd
    :<|> handleNewUser
    :<|> handleDoesUserExist
        )
  :<|> handleEpisodeNew

runDb :: DbAction a -> Handler a
runDb action = do
  pool <- asks cfgPool
  liftIO $ runSqlPool action pool

handleFeedXML :: Handler Lazy.ByteString
handleFeedXML = do
  episodeList <- runDb getAllValues
  url <- asks cfgUrl
  let contents = renderMarkup (
        let title = "full serendipity" :: Text
            img = "podcast-logo.jpg" :: Text
            imgUrl = url <> img
            description = "Wir reden hier über Themen" :: Text
            copyright = "Rubm & Luke" :: Text
            email = "luke.rubm@gmail.com (Luke & Rubm)" :: Text
            pubDate = "Thu, 17 Dec 2020 02:00:00 GMT" :: Text
            itunesSubtitle = "Wir reden hier über Themen (Subtitle)" :: Text
            itunesSummary = "Wir reden hier über Themen (Summary)" :: Text
            authors = "Luke & Rubm" :: Text
            itunesOwnerNames = "Luke and Rubm" :: Text
            episodeData = getEpisodeFeedData url <$>
              sortOn  (Down . episodeCreated) episodeList
            latestDate = maybe pubDate efdRFC822 $ headMay episodeData
        in  $(makeRelativeToProject "feed.xml.tpl" >>= compileHtmlFile))
  pure contents

handleGrantAuthPwd :: Credentials -> Handler RespLogin
handleGrantAuthPwd Credentials{..} = pure $ RespLoginFailure LoginFailureWrongPassword

handleNewUser :: Credentials -> Handler (Maybe CompactJWT)
handleNewUser Credentials{..} = pure Nothing

handleDoesUserExist :: Text -> Handler Bool
handleDoesUserExist str = pure False

handleEpisodeNew :: EpisodeNew -> Handler ()
handleEpisodeNew _ = undefined
