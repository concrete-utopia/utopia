{-# OPTIONS_GHC -fno-warn-orphans   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
{-|
  Provides some additional utilities for interacting with the servant library.
  Including appropriate mime types and accept headers for a bunch of image types.
-}
module Utopia.Web.Servant where

import           Control.Monad.Fail
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import           Data.Time.Clock
import           Data.Time.Format
import           Network.HTTP.Media       hiding (Accept)
import           Network.OAuth.OAuth2
import           Prelude                  (String)
import           Protolude
import           Servant.API
import           Servant.HTML.Blaze

data BMP

instance Accept BMP where
  contentType _ = "image" // "bmp"

instance MimeRender BMP BL.ByteString where
  mimeRender _ bytes = bytes

instance MimeUnrender BMP BL.ByteString where
  mimeUnrender _ bytes = Right bytes


data GIF

instance Accept GIF where
  contentType _ = "image" // "gif"

instance MimeRender GIF BL.ByteString where
  mimeRender _ bytes = bytes

instance MimeUnrender GIF BL.ByteString where
  mimeUnrender _ bytes = Right bytes


data JPG

instance Accept JPG where
  contentTypes _ = ("image" // "jpeg") :| ["image" // "jpg"]

instance MimeRender JPG BL.ByteString where
  mimeRender _ bytes = bytes

instance MimeUnrender JPG BL.ByteString where
  mimeUnrender _ bytes = Right bytes


data PNG

instance Accept PNG where
  contentType _ = "image" // "png"

instance MimeRender PNG BL.ByteString where
  mimeRender _ bytes = bytes

instance MimeUnrender PNG BL.ByteString where
  mimeUnrender _ bytes = Right bytes


data SVG

instance Accept SVG where
  contentType _ = "image" // "svg+xml"

instance MimeRender SVG BL.ByteString where
  mimeRender _ bytes =  bytes

instance MimeUnrender SVG BL.ByteString where
  mimeUnrender _ bytes = Right bytes

instance MimeUnrender HTML Text where
  mimeUnrender _ bytes = first (fail . show) $ decodeUtf8' $ BL.toStrict bytes

data ZIP

instance Accept ZIP where
  contentType _ = "application" // "zip"

instance MimeRender ZIP BL.ByteString where
  mimeRender _ bytes = bytes

instance MimeUnrender ZIP BL.ByteString where
  mimeUnrender _ bytes = Right bytes


data PrettyJSON

instance Accept PrettyJSON where
  contentType _ = "application" // "json"

instance ToJSON a => MimeRender PrettyJSON a where
  mimeRender _ val = encodePretty val


data ForcedJSON

instance Accept ForcedJSON where
  contentType _ = "application" // "json"

instance MimeRender ForcedJSON BL.ByteString where
  mimeRender _ bytes = bytes

instance MimeRender ForcedJSON ByteString where
  mimeRender _ bytes = BL.fromStrict bytes


newtype LastModifiedTime = LastModifiedTime { getLastModifiedTime :: UTCTime }
                           deriving (Eq, Ord, Show)

lastModifiedFormat :: String
lastModifiedFormat = "%a, %_d %b %Y %H:%M:%S %Z"

instance FromHttpApiData LastModifiedTime where
  parseUrlPiece toParse = maybe (Left ("Unable to parse " <> toParse)) (Right . LastModifiedTime) $ parseTimeM True defaultTimeLocale lastModifiedFormat (toS toParse)

instance ToHttpApiData LastModifiedTime where
  toUrlPiece (LastModifiedTime lastModifiedTime) = toS $ formatTime defaultTimeLocale lastModifiedFormat lastModifiedTime

data ProjectIdWithSuffix = ProjectIdWithSuffix Text Text
                         deriving (Eq, Ord, Show)

instance FromHttpApiData ProjectIdWithSuffix where
  parseUrlPiece toParse = fmap (\(pid, pidsuffix) -> ProjectIdWithSuffix pid (T.drop 1 pidsuffix)) $ fmap (T.breakOn "-") $ parseUrlPiece toParse

instance ToHttpApiData ProjectIdWithSuffix where
  toUrlPiece (ProjectIdWithSuffix projectId projectSuffix) = projectId <> (if T.null projectSuffix then T.empty else "-" <> projectSuffix)

instance FromHttpApiData ExchangeToken where
  parseUrlPiece toParse = fmap ExchangeToken $ parseUrlPiece toParse
