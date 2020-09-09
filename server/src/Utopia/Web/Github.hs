{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Utopia.Web.Github where

import qualified Data.ByteString.Lazy      as BL
import           Control.Lens              hiding ((.=), (<.>))
import qualified Network.Wreq              as WR
import           Protolude

fetchRepoArchive :: Text -> Text -> IO BL.ByteString
fetchRepoArchive owner repo = do
  -- https://docs.github.com/en/rest/reference/repos#download-a-repository-archive
  -- https://docs.github.com/en/rest/overview/resources-in-the-rest-api#user-agent-required
  let options = WR.defaults & WR.header "User-Agent" .~ ["concrete-utopia/utopia"] & WR.header "Accept" .~ ["application/vnd.github.v3+json"]
  let repoUrl = "https://api.github.com/repos/" <> owner <> "/" <> repo <> "/zipball/"
  repoResult <- WR.getWith options (toS repoUrl)
  return $ repoResult ^. WR.responseBody