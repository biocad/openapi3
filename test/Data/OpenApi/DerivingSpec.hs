{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.OpenApi.DerivingSpec where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON (..), Value (..), fromJSON)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

import Data.OpenApi
import Data.OpenApi.Deriving
import Data.OpenApi.Schema.Validation (validateToJSON)

import Test.Hspec

data Unwrapped

instance AesonOptions Unwrapped where
  aesonOptions _ = Aeson.defaultOptions { Aeson.unwrapUnaryRecords = True }

data LowerTags

instance AesonOptions LowerTags where
  aesonOptions _ = Aeson.defaultOptions { Aeson.constructorTagModifier = map toLower }

-- | A single-field record: with 'Unwrapped' it encodes as its bare field.
data Flags = Flags { flags :: [Bool] }
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, ToSchema) via WithAesonOptions Unwrapped Flags

data Colour = Red | Green
  deriving (Eq, Show, Generic)
  deriving (ToJSON, FromJSON, ToSchema) via WithAesonOptions LowerTags Colour

spec :: Spec
spec = describe "WithAesonOptions" $ do
  describe "unwrapUnaryRecords" $ do
    it "encodes a single-field record as its field" $
      toJSON (Flags [True]) `shouldBe` Aeson.toJSON [True]
    it "toEncoding agrees with toJSON" $
      Aeson.decode (Aeson.encode (Flags [True])) `shouldBe` Just (Aeson.toJSON [True])
    it "decodes the bare field" $
      fromJSON (Aeson.toJSON [False]) `shouldBe` Aeson.Success (Flags [False])
    it "describes the bare field in the schema" $
      toSchema (Proxy :: Proxy Flags) ^. type_ `shouldBe` Just OpenApiArray
    it "keeps the type's name" $
      schemaName (Proxy :: Proxy Flags) `shouldBe` Just "Flags"
    it "validates against its own schema" $
      validateToJSON (Flags [True, False]) `shouldBe` []

  describe "constructorTagModifier" $ do
    it "encodes the modified tag" $
      toJSON Red `shouldBe` String "red"
    it "lists the modified tags in the schema" $
      toSchema (Proxy :: Proxy Colour) ^. enum_ `shouldBe` Just ["red", "green"]
    it "validates against its own schema" $
      validateToJSON Green `shouldBe` []
