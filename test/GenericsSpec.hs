{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module GenericsSpec
  ( spec
  ) where

import           Control.Monad
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as List
import           Data.Maybe
import           Data.OpenApi
import           Data.OpenApi.Declare
import           Data.OpenApi.Internal.Schema
import           Data.OpenApi.Internal.Schema.Validation
import           Data.OpenApi.Schema
import           Data.OpenApi.Schema.Generator
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck


spec :: Spec
spec = describe "genericDeclareNamedSchema" $ do
  mkTests $ Proxy @Unit
  mkTests $ Proxy @UnaryConstructor
  mkTests $ Proxy @UnaryConstructorMaybe
  mkTests $ Proxy @UnaryRecord
  mkTests $ Proxy @UnaryRecordMaybe
  mkTests $ Proxy @ProductType
  mkTests $ Proxy @Record
  mkTests $ Proxy @SumType
  mkTests $ Proxy @()
  mkTests $ Proxy @[Int]
  mkTests $ Proxy @(Maybe Int)
  mkTests $ Proxy @(Either Bool Int)
  mkTests $ Proxy @(Char, Int)
  mkTests $ Proxy @(Char, Int, Bool)

mkTests
  :: ( Generic a, GToSchema (Rep a), Arbitrary a, Typeable a, Show a
     , Aeson.GFromJSON Zero (Rep a), Aeson.GToJSON' Value Zero (Rep a)
     )
  => Proxy a
  -> Spec
mkTests proxy = describe (show $ typeRep proxy) $ forM_ allOptsCombinations $ \opts ->
  describe (showDiff opts) $ do
    let (defs, NamedSchema _ sch) =
          runDeclare (genericDeclareNamedSchema schemaOpts proxy) mempty
        jsonOpts = toAesonOptions opts
        schemaOpts = fromAesonOptions jsonOpts
    prop "random value validates again schema" $ \val -> do
      let jsonVal = genericToJSON jsonOpts $ val `asProxyTypeOf` proxy
      case validateJSON defs sch jsonVal of
        [] -> pure ()
        errors -> expectationFailure $ unlines errors
    prop "value generated from schema is parsed successfully" $
      forAll (schemaGen defs sch) $ \val ->
        case (`asProxyTypeOf` proxy) <$> parseEither (genericParseJSON jsonOpts) val of
          Right _ -> pure ()
          Left err -> expectationFailure err

-- | Generate all possible variant of 'Aeson.Options'
allOptsCombinations :: [Opts]
allOptsCombinations = do
  snakeField <- [True, False]
  snakeConstructor <- [True, False]
  allNullaryToStringTag <- [True, False]
  omitNothingFields <- [True, False]
  sumEncoding <-
    [ Aeson.defaultTaggedObject
    , Aeson.UntaggedValue
    , Aeson.ObjectWithSingleField
    -- , Aeson.TwoElemArray FIXME
    ]
  unwrapUnaryRecords <- [True, False]
  tagSingleConstructors <- [True, False]
  rejectUnknownFields <- [True, False]
  pure Opts{..}

toAesonOptions :: Opts -> Aeson.Options
toAesonOptions Opts{..} = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = if snakeField then camelTo2 '_' else id
  , Aeson.constructorTagModifier = if snakeConstructor then camelTo2 '_' else id
  , Aeson.allNullaryToStringTag = allNullaryToStringTag
  , Aeson.omitNothingFields = omitNothingFields
  , Aeson.sumEncoding = sumEncoding
  , Aeson.unwrapUnaryRecords = unwrapUnaryRecords
  , Aeson.tagSingleConstructors = tagSingleConstructors
  , Aeson.rejectUnknownFields = rejectUnknownFields
  }

data Opts = Opts
  { snakeField :: Bool
  , snakeConstructor :: Bool
  , allNullaryToStringTag :: Bool
  , omitNothingFields :: Bool
  , sumEncoding :: Aeson.SumEncoding
  , unwrapUnaryRecords :: Bool
  , tagSingleConstructors :: Bool
  , rejectUnknownFields :: Bool
  } deriving stock (Generic)

-- Show difference with default options
showDiff :: Opts -> String
showDiff Opts{..} = if null diff then "defaultOptions" else List.intercalate ", " diff
  where
    diff = catMaybes
      [ guard snakeField >> Just "snakeField=True"
      , guard snakeConstructor >> Just "snakeConstructor=True"
      , do
          guard $ allNullaryToStringTag /= Aeson.allNullaryToStringTag Aeson.defaultOptions
          Just $ "allNullaryToStringTag=" <> show allNullaryToStringTag
      , do
          guard $ omitNothingFields /= Aeson.omitNothingFields Aeson.defaultOptions
          Just $ "omitNothingFields=" <> show omitNothingFields
      , do
          guard $ sumEncoding /= Aeson.sumEncoding Aeson.defaultOptions
          Just $ "sumEncoding=" <> show sumEncoding
      , do
          guard $ unwrapUnaryRecords /= Aeson.unwrapUnaryRecords Aeson.defaultOptions
          Just $ "unwrapUnaryRecords=" <> show unwrapUnaryRecords
      , do
          guard $ tagSingleConstructors /= Aeson.tagSingleConstructors Aeson.defaultOptions
          Just $ "tagSingleConstructors=" <> show tagSingleConstructors
      , do
          guard $ rejectUnknownFields /= Aeson.rejectUnknownFields Aeson.defaultOptions
          Just $ "rejectUnknownFields=" <> show rejectUnknownFields
      ]

data Unit = Unit
  deriving stock (Generic, Show)

instance Arbitrary Unit where
  arbitrary = pure Unit

newtype UnaryConstructor = UnaryConstructor Int
  deriving stock (Generic, Show)
  deriving newtype Arbitrary

newtype UnaryConstructorMaybe = UnaryConstructorMaybe (Maybe Int)
  deriving stock (Generic, Show)
  deriving newtype Arbitrary

newtype UnaryRecord = UnaryRecord { unaryRecord :: Int }
  deriving stock (Generic, Show)
  deriving newtype Arbitrary

newtype UnaryRecordMaybe = UnaryRecordMaybe { unaryRecordMaybe :: Maybe Int }
  deriving stock (Generic, Show)
  deriving newtype Arbitrary

data ProductType = ProductType Int Bool
  deriving stock (Generic, Show)

instance Arbitrary ProductType where
  arbitrary = ProductType <$> arbitrary <*> arbitrary

data Record = Record
  { fieldOne :: Int
  , fieldTwo :: Char
  , fieldMaybe :: Maybe Bool
  } deriving stock (Generic, Show)

instance Arbitrary Record where
  arbitrary = do
    fieldOne <- arbitrary
    fieldTwo <- arbitrary
    fieldMaybe <- arbitrary
    pure Record{..}

data SumType
  = SumNullaryConstructor
  | SumUnaryConstructor Int
  | SumUnaryConstructorWithField { sumUnaryConstructorWithField :: Int }
  | SumBinaryConstructor Int Char
  | SumBinaryConstructorWithFields
    { sumBinaryConstructorWithFields1 :: Int
    , sumBinaryConstructorWithFields2 :: Maybe Bool
    }
  deriving stock (Generic, Show)

instance Arbitrary SumType where
  arbitrary = oneof
    [ pure SumNullaryConstructor
    , SumUnaryConstructor <$> arbitrary
    , SumUnaryConstructorWithField <$> arbitrary
    , SumBinaryConstructor <$> arbitrary <*> arbitrary
    , SumBinaryConstructorWithFields <$> arbitrary <*> arbitrary
    ]
