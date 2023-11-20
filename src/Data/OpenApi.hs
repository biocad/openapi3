-- |
-- Module:      Data.OpenApi
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Swagger™ is a project used to describe and document RESTful APIs.
--
-- The Swagger specification defines a set of files required to describe such an API.
-- These files can then be used by the Swagger-UI project to display the API
-- and Swagger-Codegen to generate clients in various languages.
-- Additional utilities can also take advantage of the resulting files, such as testing tools.
module Data.OpenApi (
  -- * How to use this library
  -- $howto

  -- ** @'Monoid'@ instances
  -- $monoids

  -- ** Lenses and prisms
  -- $lens

  -- ** Schema specification
  -- $schema

  -- ** Manipulation
  -- $manipulation

  -- ** Validation
  -- $validation

  -- * Re-exports
  module Data.OpenApi.Lens,
  module Data.OpenApi.Optics,
  module Data.OpenApi.Operation,
  module Data.OpenApi.ParamSchema,
  module Data.OpenApi.Schema,
  module Data.OpenApi.Schema.Validation,

  -- * Swagger specification
  OpenApi(..),
  Server(..),
  ServerVariable(..),
  Components(..),

  -- ** Info types
  Info(..),
  Contact(..),
  License(..),

  -- ** PathItem
  PathItem(..),

  -- ** Operations
  Operation(..),
  Tag(..),
  TagName,

  -- ** Types and formats
  OpenApiType(..),
  Format,
  Definitions,
  Style(..),

  -- ** Parameters
  Param(..),
  ParamLocation(..),
  ParamName,
  Header(..),
  HeaderName,
  Example(..),
  RequestBody(..),
  MediaTypeObject(..),
  Encoding(..),

  -- ** Schemas
  Schema(..),
  NamedSchema(..),
  OpenApiItems(..),
  Xml(..),
  Pattern,
  AdditionalProperties(..),
  Discriminator(..),

  -- ** Responses
  Responses(..),
  Response(..),
  HttpStatusCode,
  Link(..),
  Callback(..),

  -- ** Security
  SecurityScheme(..),
  SecuritySchemeType(..),
  HttpSchemeType(..),
  SecurityDefinitions(..),
  SecurityRequirement(..),

  -- *** API key
  ApiKeyParams(..),
  ApiKeyLocation(..),

  -- *** OAuth2
  OAuth2Flows(..),
  OAuth2Flow(..),
  OAuth2ImplicitFlow(..),
  OAuth2PasswordFlow(..),
  OAuth2ClientCredentialsFlow(..),
  OAuth2AuthorizationCodeFlow(..),
  AuthorizationURL,
  TokenURL,

  -- ** External documentation
  ExternalDocs(..),

  -- ** References
  Reference(..),
  Referenced(..),

  -- ** Miscellaneous
  MimeList(..),
  URL(..),
) where

import Data.OpenApi.Lens
import Data.OpenApi.Optics ()
import Data.OpenApi.Operation
import Data.OpenApi.ParamSchema
import Data.OpenApi.Schema
import Data.OpenApi.Schema.Validation

import Data.OpenApi.Internal

-- $setup
-- >>> import Control.Lens
-- >>> import Data.Aeson
-- >>> import Data.Monoid
-- >>> import Data.Proxy
-- >>> import GHC.Generics
-- >>> import qualified Data.ByteString.Lazy.Char8 as BSL
-- >>> import Data.OpenApi.Internal
-- >>> import Data.OpenApi.Internal.Schema
-- >>> import qualified Data.HashMap.Strict.InsOrd as IOHM
-- >>> import Data.OpenApi.Internal.Utils
-- >>> import Data.OpenApi.Lens
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
-- >>> :set -fno-warn-missing-methods

-- $howto
--
-- This section explains how to use this library to work with Swagger specification.

-- $monoids
--
-- Virtually all types representing Swagger specification have @'Monoid'@ instances.
-- The @'Monoid'@ type class provides two methods — @'mempty'@ and @'mappend'@.
--
-- In this library you can use @'mempty'@ for a default/empty value. For instance:
--
-- >>> BSL.putStrLn $ encodePretty (mempty :: OpenApi)
-- {
--     "components": {},
--     "info": {
--         "title": "",
--         "version": ""
--     },
--     "openapi": "3.0.0",
--     "paths": {}
-- }
--
-- As you can see some spec properties (e.g. @"version"@) are there even when the spec is empty.
-- That is because these properties are actually required ones.
--
-- You /should/ always override the default (empty) value for these properties,
-- although it is not strictly necessary:
--
-- >>> BSL.putStrLn $ encodePretty mempty { _infoTitle = "Todo API", _infoVersion = "1.0" }
-- {
--     "title": "Todo API",
--     "version": "1.0"
-- }
--
-- You can merge two values using @'mappend'@ or its infix version @('<>')@:
--
-- >>> BSL.putStrLn $ encodePretty $ mempty { _infoTitle = "Todo API" } <> mempty { _infoVersion = "1.0" }
-- {
--     "title": "Todo API",
--     "version": "1.0"
-- }

--
-- This can be useful for combining specifications of endpoints into a whole API specification:
--
-- @
-- \-\- /account subAPI specification
-- accountAPI :: OpenApi
--
-- \-\- /task subAPI specification
-- taskAPI :: OpenApi
--
-- \-\- while API specification is just a combination
-- \-\- of subAPIs' specifications
-- api :: OpenApi
-- api = accountAPI <> taskAPI
-- @

-- $lens
--
-- Note: if you're working with the <https://hackage.haskell.org/package/optics optics> library, take a look at "Data.OpenApi.Optics".
--
-- Since @'Swagger'@ has a fairly complex structure, lenses and prisms are used
-- to work comfortably with it. In combination with @'Monoid'@ instances, lenses
-- make it fairly simple to construct/modify any part of the specification:
--
-- >>> :{
-- BSL.putStrLn $ encodePretty $ (mempty :: OpenApi)
--   & components . schemas .~ IOHM.fromList [ ("User", mempty & type_ ?~ OpenApiString) ]
--   & paths .~
--     IOHM.fromList [ ("/user", mempty & get ?~ (mempty
--         & at 200 ?~ ("OK" & _Inline.content.at "application/json" ?~ (mempty & schema ?~ Ref (Reference "User")))
--         & at 404 ?~ "User info not found")) ]
-- :}
-- {
--     "components": {
--         "schemas": {
--             "User": {
--                 "type": "string"
--             }
--         }
--     },
--     "info": {
--         "title": "",
--         "version": ""
--     },
--     "openapi": "3.0.0",
--     "paths": {
--         "/user": {
--             "get": {
--                 "responses": {
--                     "200": {
--                         "content": {
--                             "application/json": {
--                                 "schema": {
--                                     "$ref": "#/components/schemas/User"
--                                 }
--                             }
--                         },
--                         "description": "OK"
--                     },
--                     "404": {
--                         "description": "User info not found"
--                     }
--                 }
--             }
--         }
--     }
-- }
--
-- In the snippet above we declare an API with a single path @/user@. This path provides method @GET@
-- which produces @application/json@ output. It should respond with code @200@ and body specified
-- by schema @User@ which is defined in @'definitions'@ property of swagger specification.
-- Alternatively it may respond with code @404@ meaning that user info is not found.
--
-- For convenience, @swagger2@ uses /classy field lenses/. It means that
-- field accessor names can be overloaded for different types. One such
-- common field is @'description'@. Many components of a Swagger specification
-- can have descriptions, and you can use the same name for them:
--
-- >>> BSL.putStrLn $ encodePretty $ (mempty :: Response) & description .~ "No content"
-- {
--     "description": "No content"
-- }
-- >>> :{
-- BSL.putStrLn $ encodePretty $ (mempty :: Schema)
--   & type_       ?~ OpenApiBoolean
--   & description ?~ "To be or not to be"
-- :}
-- {
--     "description": "To be or not to be",
--     "type": "boolean"
-- }
--
-- Additionally, to simplify working with @'Response'@, both @'Operation'@ and @'Responses'@
-- have direct access to it via @'at' code@. Example:
--
-- >>> :{
-- BSL.putStrLn $ encodePretty $ (mempty :: Operation)
--   & at 404 ?~ "Not found"
-- :}
-- {
--     "responses": {
--         "404": {
--             "description": "Not found"
--         }
--     }
-- }
--
-- You might've noticed that @'type_'@ has an extra underscore in its name
-- compared to, say, @'description'@ field accessor.
-- This is because @type@ is a keyword in Haskell.
-- A few other field accessors are modified in this way:
--
--    - @'in_'@, @'type_'@, @'default_'@ (as keywords);
--    - @'maximum_'@, @'minimum_'@, @'head_'@ (as conflicting with @Prelude@);
--    - @'enum_'@ (as conflicting with @Control.Lens@).

-- $schema
--
-- @'ParamSchema'@ and @'Schema'@ are the two core types for data model specification.
--
-- @'ParamSchema' t@ specifies all the common properties, available for every data schema.
-- The @t@ parameter imposes some restrictions on @type@ and @items@ properties (see @'OpenApiType'@ and @'OpenApiItems'@).
--
-- @'Schema'@ is used for request and response bodies and allows specifying objects
-- with properties in addition to what @'ParamSchema'@ provides.
--
-- In most cases you will have a Haskell data type for which you would like to
-- define a corresponding schema. To facilitate this use case
-- @swagger2@ provides two classes for schema encoding.
-- Both these classes provide means to encode /types/ as Swagger /schemas/.
--
-- @'ToParamSchema'@ is intended to be used for primitive API endpoint parameters,
-- such as query parameters, headers and URL path pieces.
-- Its corresponding value-encoding class is @'ToHttpApiData'@ (from @http-api-data@ package).
--
-- @'ToSchema'@ is used for request and response bodies and mostly differ from
-- primitive parameters by allowing objects/mappings in addition to primitive types and arrays.
-- Its corresponding value-encoding class is @'ToJSON'@ (from @aeson@ package).
--
-- While lenses and prisms make it easy to define schemas, it might be that you don't need to:
-- @'ToSchema'@ and @'ToParamSchema'@ classes both have default @'Generic'@-based implementations!
--
-- @'ToSchema'@ default implementation is also aligned with @'ToJSON'@ default implementation with
-- the only difference being for sum encoding. @'ToJSON'@ defaults sum encoding to @'defaultTaggedObject'@,
-- while @'ToSchema'@ defaults to something which corresponds to @'ObjectWithSingleField'@. This is due to
-- @'defaultTaggedObject'@ behavior being hard to specify in Swagger.
--
-- Here's an example showing @'ToJSON'@–@'ToSchema'@ correspondance:
--
-- >>> data Person = Person { name :: String, age :: Integer } deriving Generic
-- >>> instance ToJSON Person
-- >>> instance ToSchema Person
-- >>> BSL.putStrLn $ encodePretty (Person "David" 28)
-- {
--     "age": 28,
--     "name": "David"
-- }
-- >>> BSL.putStrLn $ encodePretty $ toSchema (Proxy :: Proxy Person)
-- {
--     "properties": {
--         "age": {
--             "type": "integer"
--         },
--         "name": {
--             "type": "string"
--         }
--     },
--     "required": [
--         "name",
--         "age"
--     ],
--     "type": "object"
-- }
--
-- This package implements OpenAPI 3.0 spec, which supports @oneOf@ in schemas, allowing any sum types
-- to be faithfully represented. All sum encodings supported by @aeson@ are supported here as well, with
-- an exception of 'Data.Aeson.TwoElemArray', since OpenAPI spec does not support heterogeneous arrays.
--
-- An example with 'Data.Aeson.TaggedObject' encoding:
--
-- >>> data Error = ErrorNoUser { userId :: Int } | ErrorAccessDenied { requiredPermission :: String } deriving Generic
-- >>> instance ToJSON Error
-- >>> instance ToSchema Error
-- >>> BSL.putStrLn $ encodePretty $ toSchema (Proxy :: Proxy Error)
-- {
--     "oneOf": [
--         {
--             "properties": {
--                 "tag": {
--                     "enum": [
--                         "ErrorNoUser"
--                     ],
--                     "type": "string"
--                 },
--                 "userId": {
--                     "maximum": 9223372036854775807,
--                     "minimum": -9223372036854775808,
--                     "type": "integer"
--                 }
--             },
--             "required": [
--                 "userId",
--                 "tag"
--             ],
--             "title": "ErrorNoUser",
--             "type": "object"
--         },
--         {
--             "properties": {
--                 "requiredPermission": {
--                     "type": "string"
--                 },
--                 "tag": {
--                     "enum": [
--                         "ErrorAccessDenied"
--                     ],
--                     "type": "string"
--                 }
--             },
--             "required": [
--                 "requiredPermission",
--                 "tag"
--             ],
--             "title": "ErrorAccessDenied",
--             "type": "object"
--         }
--     ]
-- }

-- $manipulation
-- Sometimes you have to work with an imported or generated @'Swagger'@.
-- For instance, <servant-swagger http://hackage.haskell.org/package/servant-swagger> generates basic @'Swagger'@
-- for a type-level servant API.
--
-- Lenses and prisms can be used to manipulate such specification to add additional information, tags, extra responses, etc.
-- To facilitate common needs, @"Data.OpenApi.Operation"@ module provides useful helpers.

-- $validation
-- While @'ToParamSchema'@ and @'ToSchema'@ provide means to easily obtain schemas for Haskell types,
-- there is no static mechanism to ensure those instances correspond to the @'ToHttpApiData'@ or @'ToJSON'@ instances.
--
-- @"Data.OpenApi.Schema.Validation"@ addresses @'ToJSON'@/@'ToSchema'@ validation.
