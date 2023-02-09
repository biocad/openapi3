-- |
-- Module:      Data.OpenApi.Schema.Validation
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Validate JSON values with Swagger Schema.
module Data.OpenApi.Schema.Validation (
  -- * How to use validation
  -- $howto

  -- ** Examples
  -- $examples

  -- ** Validating @'Maybe'@
  -- $maybe

  -- * JSON validation

  ValidationError,

  -- ** Using 'ToJSON' and 'ToSchema'
  validatePrettyToJSON,
  validateToJSON,
  validateToJSONWithPatternChecker,
  renderValidationErrors,

  -- ** Using 'Value' and 'Schema'
  validateJSON,
  validateJSONWithPatternChecker,
) where

import Data.OpenApi.Internal.Schema.Validation

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Aeson
-- >>> import Data.Proxy
-- >>> import Data.OpenApi
-- >>> import GHC.Generics
-- >>> :set -XDeriveGeneric

-- $howto
--
-- This module provides helpful functions for JSON validation.
-- These functions are meant to be used in test suites for your application
-- to ensure that JSON respresentation for your data corresponds to
-- schemas you're using for the Swagger specification.
--
-- It is recommended to use validation functions as QuickCheck properties
-- (see <http://hackage.haskell.org/package/QuickCheck>).

-- $examples
--
-- >>> validateToJSON "hello"
-- []
--
-- >>> validateToJSON False
-- []
--
-- >>> newtype Nat = Nat Integer deriving Generic
-- >>> instance ToJSON Nat where toJSON (Nat n) = toJSON n
-- >>> instance ToSchema Nat where declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy & mapped.minimum_ ?~ 0
-- >>> validateToJSON (Nat 10)
-- []
-- >>> validateToJSON (Nat (-5))
-- ["value -5.0 falls below minimum (should be >=0.0)"]

-- $maybe
--
-- Because @'Maybe' a@ has the same schema as @a@, validation
-- generally fails for @null@ JSON:
--
-- >>> validateToJSON (Nothing :: Maybe String)
-- ["expected JSON value of type OpenApiString"]
-- >>> validateToJSON ([Just "hello", Nothing] :: [Maybe String])
-- ["expected JSON value of type OpenApiString"]
-- >>> validateToJSON (123, Nothing :: Maybe String)
-- ["Value not valid under any of 'anyOf' schemas: Null"]
--
-- However, when @'Maybe' a@ is a type of a record field,
-- validation takes @'required'@ property of the @'Schema'@
-- into account:
--
-- >>> data Person = Person { name :: String, age :: Maybe Int } deriving Generic
-- >>> instance ToJSON Person
-- >>> instance ToSchema Person
-- >>> validateToJSON (Person "Nick" (Just 24))
-- []
-- >>> validateToJSON (Person "Nick" Nothing)
-- []
