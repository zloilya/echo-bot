module Common where

import Data.Aeson.Types
  ( GFromJSON,
    GToJSON,
    Options (fieldLabelModifier, omitNothingFields),
    Parser,
    Value,
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.List (drop)
import GHC.Generics (Generic (Rep))

-- | Method used to drop prefix from field name during serialization
toJsonDrop :: forall a. (Generic a, GToJSON Zero (Rep a)) => Int -> a -> Value
toJsonDrop prefix =
  genericToJSON
    defaultOptions
      { fieldLabelModifier = drop prefix,
        omitNothingFields = True
      }

-- | Method used to drop prefix from field name during deserialization
parseJsonDrop :: forall a. (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseJsonDrop prefix = genericParseJSON defaultOptions {fieldLabelModifier = drop prefix}
