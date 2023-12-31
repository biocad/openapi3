module SpecCommon where

import Data.Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import qualified Data.Vector as Vector

import Data.OpenApi.Internal.Utils
import Test.Hspec

(<=>) :: (Eq a, Show a, ToJSON a, FromJSON a, HasCallStack) => a -> Value -> Spec
x <=> js = do
  it "encodes correctly" $ do
    encodePretty x `shouldBe` encodePretty js
  it "decodes correctly" $ case fromJSON js of
    Success expected -> encodePretty x `shouldBe` encodePretty (expected `asTypeOf` x)
    Error err -> expectationFailure err
  it "roundtrips: eitherDecode . encode" $ do
    eitherDecode (encode x) `shouldBe` Right x
  it "roundtrips with toJSON" $ do
    eitherDecode (encode $ toJSON x) `shouldBe` Right x
  it "roundtrips with toEncoding" $ do
    eitherDecode (toLazyByteString $ fromEncoding $ toEncoding x) `shouldBe` Right x
