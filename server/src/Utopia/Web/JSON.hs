module Utopia.Web.JSON where

import           Data.Aeson
import           Protolude

jsonOptions :: Options
jsonOptions = defaultOptions{fieldLabelModifier = drop 1, sumEncoding = defaultTaggedObject{tagFieldName = "type"}}
