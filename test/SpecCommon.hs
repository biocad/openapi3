{-# LANGUAGE CPP #-}

module SpecCommon where

import Data.Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Vector as Vector

import Test.Hspec

isSubJSON :: Value -> Value -> Bool
isSubJSON Null _ = True
isSubJSON (Object x) (Object y) = keysCompat x == keysCompat i && F.and i
  where
    i = intersectionWithCompat isSubJSON x y
isSubJSON (Array xs) (Array ys) = Vector.length xs == Vector.length ys && F.and (Vector.zipWith isSubJSON xs ys)
isSubJSON x y = x == y

(<=>) :: (Eq a, Show a, ToJSON a, FromJSON a, HasCallStack) => a -> Value -> Spec
x <=> js = do
  it "encodes correctly" $ do
    toJSON x `shouldBe` js
  it "decodes correctly" $ do
    fromJSON js `shouldBe` Success x
  it "roundtrips: eitherDecode . encode" $ do
    eitherDecode (encode x) `shouldBe` Right x
  it "roundtrips with toJSON" $ do
    eitherDecode (encode $ toJSON x) `shouldBe` Right x
  it "roundtrips with toEncoding" $ do
    eitherDecode (toLazyByteString $ fromEncoding $ toEncoding x) `shouldBe` Right x

-------------------------------------------------------------------------------
-- Aeson compatibility helpers
-------------------------------------------------------------------------------

#if MIN_VERSION_aeson(2,0,0)
keysCompat :: KeyMap.KeyMap v -> [Text]
keysCompat = map Key.toText . KeyMap.keys
#else
keysCompat :: HashMap.HashMap Text v -> [Text]
keysCompat = HashMap.keys
#endif

#if MIN_VERSION_aeson(2,0,0)
intersectionWithCompat ::
  (a -> b -> c) -> KeyMap.KeyMap a -> KeyMap.KeyMap b -> KeyMap.KeyMap c
intersectionWithCompat = KeyMap.intersectionWith
#else
intersectionWithCompat ::
  (a -> b -> c) ->
  HashMap.HashMap Text a ->
  HashMap.HashMap Text b ->
  HashMap.HashMap Text c
intersectionWithCompat = HashMap.intersectionWith
#endif
