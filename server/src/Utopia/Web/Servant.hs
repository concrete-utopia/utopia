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

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as BL
import           Network.HTTP.Media       hiding (Accept)
import           Protolude
import           Servant.API


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


data PrettyJSON

instance Accept PrettyJSON where
  contentType _ = "application" // "json"

instance ToJSON a => MimeRender PrettyJSON a where
  mimeRender _ val = encodePretty val


data ForcedJSON

instance Accept ForcedJSON where
  contentType _ = "application" // "json"

instance MimeRender ForcedJSON BL.ByteString where
  mimeRender _ bytes =  bytes


