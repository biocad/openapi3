module SpecCommon where

import Data.Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import Test.Hspec

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
