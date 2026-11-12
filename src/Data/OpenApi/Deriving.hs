-- |
-- Module:      Data.OpenApi.Deriving
-- Stability:   experimental
--
-- Derive @'ToJSON'@, @'FromJSON'@ and @'ToSchema'@ from one set of Aeson
-- 'Aeson.Options', so that the schema describes the JSON the type actually
-- encodes to.
--
-- Writing the generic instances by hand means passing the same options twice,
-- once to 'Aeson.genericToJSON' and once, through 'fromAesonOptions', to
-- 'genericDeclareNamedSchema'. Nothing checks that the two agree: change one
-- and the schema quietly stops matching the wire format. 'WithAesonOptions'
-- names the options once, at the type level, and derives all three instances
-- from them:
--
-- @
-- {-# LANGUAGE DerivingVia #-}
--
-- data Unwrapped
--
-- instance 'AesonOptions' Unwrapped where
--   aesonOptions _ = Aeson.defaultOptions { Aeson.unwrapUnaryRecords = True }
--
-- newtype History = History { entries :: [Int] }
--   deriving ('Generic')
--   deriving ('ToJSON', 'FromJSON', 'ToSchema') via 'WithAesonOptions' Unwrapped History
-- @
module Data.OpenApi.Deriving (
  AesonOptions(..),
  WithAesonOptions(..),
) where

import Prelude ()
import Prelude.Compat

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (GFromJSON, GToEncoding, GToJSON, Zero)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Rep)

import Data.OpenApi.Internal.Schema (GToSchema, ToSchema (..), genericDeclareNamedSchema)
import Data.OpenApi.SchemaOptions (fromAesonOptions)

-- | A type-level name for a set of Aeson 'Aeson.Options', for use with
-- 'WithAesonOptions'. The tag is usually an empty data type.
class AesonOptions (tag :: Type) where
  aesonOptions :: Proxy tag -> Aeson.Options

-- | A @DerivingVia@ target: @'WithAesonOptions' tag a@ has generic
-- @'ToJSON'@, @'FromJSON'@ and @'ToSchema'@ instances for @a@, all built from
-- @'aesonOptions' ('Proxy' :: 'Proxy' tag)@. The schema options come from
-- 'fromAesonOptions'.
newtype WithAesonOptions (tag :: Type) a = WithAesonOptions a

instance (AesonOptions tag, Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a))
  => ToJSON (WithAesonOptions tag a) where
  toJSON (WithAesonOptions a) = Aeson.genericToJSON (aesonOptions (Proxy :: Proxy tag)) a
  toEncoding (WithAesonOptions a) = Aeson.genericToEncoding (aesonOptions (Proxy :: Proxy tag)) a

instance (AesonOptions tag, Generic a, GFromJSON Zero (Rep a))
  => FromJSON (WithAesonOptions tag a) where
  parseJSON = fmap WithAesonOptions . Aeson.genericParseJSON (aesonOptions (Proxy :: Proxy tag))

instance (AesonOptions tag, Typeable tag, Generic a, GToSchema (Rep a), Typeable a)
  => ToSchema (WithAesonOptions tag a) where
  declareNamedSchema _ =
    genericDeclareNamedSchema (fromAesonOptions (aesonOptions (Proxy :: Proxy tag))) (Proxy :: Proxy a)
