-- |
-- Module:      Data.OpenApi.ParamSchema
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Types and functions for working with Swagger parameter schema.
module Data.OpenApi.ParamSchema (
  -- * Encoding
  ToParamSchema(..),

  -- * Generic schema encoding
  genericToParamSchema,
  toParamSchemaBoundedIntegral,

  -- * Schema templates
  passwordSchema,
  binarySchema,
  byteSchema,

  -- * Generic encoding configuration
  SchemaOptions(..),
  defaultSchemaOptions,
) where

import Data.OpenApi.Internal.ParamSchema
import Data.OpenApi.SchemaOptions
