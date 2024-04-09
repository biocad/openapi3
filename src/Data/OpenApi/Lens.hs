{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module:      Data.OpenApi.Lens
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Lenses and prisms for Swagger.
module Data.OpenApi.Lens where

import Control.Lens
import Data.Aeson (Value)
import Data.Scientific (Scientific)
import Data.OpenApi.Internal
import Data.OpenApi.Internal.Utils
import Data.Text (Text)

-- * Classy lenses

makeFields ''OpenApi
makeFields ''Components
makeFields ''Server
makeFields ''RequestBody
makeFields ''MediaTypeObject
makeFields ''Info
makeFields ''Contact
makeFields ''License
makeLensesWith swaggerFieldRules ''PathItem
makeFields ''Tag
makeFields ''Operation
makeLensesWith swaggerFieldRules ''Param
makeFields ''Header
makeLensesWith swaggerFieldRules ''Schema
makeFields ''NamedSchema
makeFields ''Xml
makeLensesWith swaggerFieldRules ''Responses
makeFields ''Response
makeLensesWith swaggerFieldRules ''SecurityScheme
makeFields ''ApiKeyParams
makeFields ''OAuth2ImplicitFlow
makeFields ''OAuth2PasswordFlow
makeFields ''OAuth2ClientCredentialsFlow
makeFields ''OAuth2AuthorizationCodeFlow
makeFields ''OAuth2Flow
makeFields ''OAuth2Flows
makeFields ''ExternalDocs
makeFields ''Encoding
makeFields ''Example
makeFields ''Discriminator
makeFields ''Link

-- * Prisms
-- ** 'SecuritySchemeType' prisms
makePrisms ''SecuritySchemeType
-- ** 'Referenced' prisms
makePrisms ''Referenced

-- ** 'OpenApiItems' prisms

_OpenApiItemsArray :: Review OpenApiItems [Referenced Schema]
_OpenApiItemsArray
  = unto (\x -> OpenApiItemsArray x)
{- \x -> case x of
      OpenApiItemsPrimitive c p -> Left (OpenApiItemsPrimitive c p)
      OpenApiItemsObject o      -> Left (OpenApiItemsObject o)
      OpenApiItemsArray a       -> Right a
-}

_OpenApiItemsObject :: Review OpenApiItems (Referenced Schema)
_OpenApiItemsObject
  = unto (\x -> OpenApiItemsObject x)
{- \x -> case x of
      OpenApiItemsPrimitive c p -> Left (OpenApiItemsPrimitive c p)
      OpenApiItemsObject o      -> Right o
      OpenApiItemsArray a       -> Left (OpenApiItemsArray a)
-}

-- =============================================================
-- More helpful instances for easier access to schema properties

type instance Index Responses = HttpStatusCode
type instance Index Operation = HttpStatusCode
type instance Index SpecificationExtensions = Text

type instance IxValue Responses = Referenced Response
type instance IxValue Operation = Referenced Response
type instance IxValue SpecificationExtensions = Value

instance Ixed Responses where ix n = responses . ix n
instance At   Responses where at n = responses . at n

instance Ixed Operation where ix n = responses . ix n
instance At   Operation where at n = responses . at n

instance Ixed SpecificationExtensions where
  ix n = coerced @_ @_ @(Definitions Value) . ix n
instance At   SpecificationExtensions where
  at n = coerced @_ @_ @(Definitions Value) . at n

instance HasType NamedSchema (Maybe OpenApiType) where type_ = schema.type_

-- OVERLAPPABLE instances

instance
  {-# OVERLAPPABLE #-}
  HasSchema s Schema
  => HasFormat s (Maybe Format) where
  format = schema.format

instance
  {-# OVERLAPPABLE #-}
  HasSchema s Schema
  => HasItems s (Maybe OpenApiItems) where
  items = schema.items

instance
  {-# OVERLAPPABLE #-}
  HasSchema s Schema
  => HasMaximum s (Maybe Scientific) where
  maximum_ = schema.maximum_

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasExclusiveMaximum s (Maybe Bool) where
  exclusiveMaximum = schema.exclusiveMaximum

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasMinimum s (Maybe Scientific) where
  minimum_ = schema.minimum_

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasExclusiveMinimum s (Maybe Bool) where
  exclusiveMinimum = schema.exclusiveMinimum

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasMaxLength s (Maybe Integer) where
  maxLength = schema.maxLength

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasMinLength s (Maybe Integer) where
  minLength = schema.minLength

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasPattern s (Maybe Text) where
  pattern = schema.pattern

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasMaxItems s (Maybe Integer) where
  maxItems = schema.maxItems

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasMinItems s (Maybe Integer) where
  minItems = schema.minItems

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasUniqueItems s (Maybe Bool) where
  uniqueItems = schema.uniqueItems

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasEnum s (Maybe [Value]) where
  enum_ = schema.enum_

instance {-# OVERLAPPABLE #-} HasSchema s Schema
  => HasMultipleOf s (Maybe Scientific) where
  multipleOf = schema.multipleOf
