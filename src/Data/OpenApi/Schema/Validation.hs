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
-- >>> import Data.Aeson.QQ.Simple
-- >>> import Data.Proxy
-- >>> import Data.OpenApi
-- >>> import Data.OpenApi.Declare
-- >>> import GHC.Generics
-- >>> :set -XDeriveGeneric
-- >>> :set -XQuasiQuotes

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
-- The behavior is in line with "aeson" behavior for derived instances.
-- When @'Maybe' a@ is a type of a record field,
-- validation accepts both ommited field and null as a field value:
--
-- >>> data Person = Person { name :: String, age :: Maybe Int } deriving Generic
-- >>> instance ToJSON Person
-- >>> instance ToSchema Person
-- >>> let (defs, sch) = runDeclare (declareSchema (Proxy :: Proxy Person)) mempty
-- >>> let validate = validateJSON defs sch
-- >>> validate [aesonQQ|{"name" : "Nick", "age" : 18}|]
-- []
-- >>> validate [aesonQQ|{"name" : "Nick", "age" : null}|]
-- []
-- >>> validate [aesonQQ|{"name" : "Nick"}|]
-- []
