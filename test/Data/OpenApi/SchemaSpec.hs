{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.OpenApi.SchemaSpec where

import Prelude ()
import Prelude.Compat

import Control.Lens ((^.))
import Data.Aeson (Value(..))
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.Proxy
import Data.Set (Set)
import qualified Data.Text as Text

import Data.OpenApi
import Data.OpenApi.Declare

import Data.OpenApi.CommonTestTypes
import SpecCommon
import Test.Hspec

import qualified Data.HashMap.Strict as HM
import Data.Time.LocalTime

checkToSchema :: (HasCallStack, ToSchema a) => Proxy a -> Value -> Spec
checkToSchema proxy js = toSchema proxy <=> js

checkSchemaName :: (HasCallStack, ToSchema a) => Maybe String -> Proxy a -> Spec
checkSchemaName sname proxy =
  it ("schema name is " ++ show sname) $
    schemaName proxy `shouldBe` fmap Text.pack sname

checkDefs :: (HasCallStack, ToSchema a) => Proxy a -> [String] -> Spec
checkDefs proxy names =
  it ("uses these definitions " ++ show names) $
    InsOrdHashMap.keys defs `shouldBe` map Text.pack names
  where
    defs = execDeclare (declareNamedSchema proxy) mempty

checkProperties :: (HasCallStack, ToSchema a) => Proxy a -> [String] -> Spec
checkProperties proxy names =
  it ("has these fields in order " ++ show names) $
    InsOrdHashMap.keys fields `shouldBe` map Text.pack names
  where
    fields = toSchema proxy ^. properties

checkInlinedSchema :: (HasCallStack, ToSchema a) => Proxy a -> Value -> Spec
checkInlinedSchema proxy js = toInlinedSchema proxy <=> js

checkInlinedSchemas :: (HasCallStack, ToSchema a) => [String] -> Proxy a -> Value -> Spec
checkInlinedSchemas names proxy js = inlineSchemas (map Text.pack names) defs s <=> js
  where
    (defs, s) = runDeclare (declareSchema proxy) mempty

checkInlinedRecSchema :: (HasCallStack, ToSchema a) => Proxy a -> Value -> Spec
checkInlinedRecSchema proxy js = inlineNonRecursiveSchemas defs s <=> js
  where
    (defs, s) = runDeclare (declareSchema proxy) mempty

checkToSchemaDeclare :: (HasCallStack, ToSchema a) => Proxy a -> Value -> Spec
checkToSchemaDeclare proxy js = runDeclare (declareSchemaRef proxy) mempty <=> js

spec :: Spec
spec = do
  describe "Generic ToSchema" $ do
    context "Unit" $ checkToSchema (Proxy :: Proxy Unit) unitSchemaJSON
    context "Person" $ checkToSchema (Proxy :: Proxy Person) personSchemaJSON
    context "ISPair" $ checkToSchema (Proxy :: Proxy ISPair) ispairSchemaJSON
    context "Either String Int" $ checkToSchema (Proxy :: Proxy EitherStringInt) eitherSchemaJSON
    context "ISHomogeneousPair" $ checkToSchema (Proxy :: Proxy ISHomogeneousPair) ishomogeneouspairSchemaJSON
    context "PairWithRef" $ checkToSchema (Proxy :: Proxy PairWithRef) pairwithrefSchemaJSON
    context "Point (fieldLabelModifier)" $ checkToSchema (Proxy :: Proxy Point) pointSchemaJSON
    context "Point5 (many field record)" $ do
      checkToSchema (Proxy :: Proxy Point5) point5SchemaJSON
      checkProperties (Proxy :: Proxy Point5) point5Properties
    context "Color (bounded enum)" $ checkToSchema (Proxy :: Proxy Color) colorSchemaJSON
    context "Shade (paramSchemaToNamedSchema)" $ checkToSchema (Proxy :: Proxy Shade) shadeSchemaJSON
    context "Paint (record with bounded enum field)" $ checkToSchema (Proxy :: Proxy Paint) paintSchemaJSON
    context "UserGroup (set newtype)" $ checkToSchema (Proxy :: Proxy UserGroup) userGroupSchemaJSON
    context "Unary records" $ do
      context "Email (unwrapUnaryRecords)"  $ checkToSchema (Proxy :: Proxy Email)  emailSchemaJSON
      context "UserId (non-record newtype)" $ checkToSchema (Proxy :: Proxy UserId) userIdSchemaJSON
      context "Player (unary record)" $ checkToSchema (Proxy :: Proxy Player) playerSchemaJSON
      context "SingleMaybeField (unary record with Maybe)" $ checkToSchema (Proxy :: Proxy SingleMaybeField) singleMaybeFieldSchemaJSON
      context "Painter (record with an optional reference)" $ checkToSchema (Proxy :: Proxy Painter) painterSchemaJSON
      context "Natural Language (single field data with recursive fields)" $ checkToSchemaDeclare (Proxy :: Proxy Predicate) predicateSchemaDeclareJSON
    context "Players (inlining schema)" $ checkToSchema (Proxy :: Proxy Players) playersSchemaJSON
    context "MyRoseTree (datatypeNameModifier)" $ checkToSchema (Proxy :: Proxy MyRoseTree) myRoseTreeSchemaJSON
    context "MyRoseTree' (datatypeNameModifier)" $ checkToSchema (Proxy :: Proxy MyRoseTree') myRoseTreeSchemaJSON'
    context "Sum types" $ do
      context "Status (sum of unary constructors)" $ checkToSchema (Proxy :: Proxy Status) statusSchemaJSON
      context "Character (ref and record sum)" $ checkToSchema (Proxy :: Proxy Character) characterSchemaJSON
      context "Light (sum with unwrapUnaryRecords)" $ checkToSchema (Proxy :: Proxy Light) lightSchemaJSON
    context "UnsignedInts" $ checkToSchema (Proxy :: Proxy UnsignedInts) unsignedIntsSchemaJSON
    context "Schema name" $ do
      context "String" $ checkSchemaName Nothing (Proxy :: Proxy String)
      context "(Int, Float)" $ checkSchemaName Nothing (Proxy :: Proxy (Int, Float))
      context "Person" $ checkSchemaName (Just "Person") (Proxy :: Proxy Person)
      context "Shade" $ checkSchemaName (Just "Shade") (Proxy :: Proxy Shade)
      context "Player (polymorphic record)" $ checkSchemaName (Just "PlayerPoly__40_PointG_Int_41_") (Proxy :: Proxy (PlayerPoly (PointG Int)))
  describe "Generic Definitions" $ do
    context "Unit" $ checkDefs (Proxy :: Proxy Unit) []
    context "Paint" $ checkDefs (Proxy :: Proxy Paint) ["Color"]
    context "Light" $ checkDefs (Proxy :: Proxy Light) ["Color"]
    context "Character" $ checkDefs (Proxy :: Proxy Character) ["Player", "Point"]
    context "MyRoseTree" $ checkDefs (Proxy :: Proxy MyRoseTree) ["RoseTree"]
    context "MyRoseTree'" $ checkDefs (Proxy :: Proxy MyRoseTree') ["myrosetree'"]
    context "[Set (Unit, Maybe Color)]" $ checkDefs (Proxy :: Proxy [Set (Unit, Maybe Color)]) ["Unit", "Color"]
    context "ResourceId" $ checkDefs (Proxy :: Proxy ResourceId) []
  describe "Inlining Schemas" $ do
    context "Paint" $ checkInlinedSchema (Proxy :: Proxy Paint) paintInlinedSchemaJSON
    context "Character" $ checkInlinedSchema (Proxy :: Proxy Character) characterInlinedSchemaJSON
    context "Character (inlining only Player)" $ checkInlinedSchemas ["Player"] (Proxy :: Proxy Character) characterInlinedPlayerSchemaJSON
    context "Light" $ checkInlinedSchema (Proxy :: Proxy Light) lightInlinedSchemaJSON
    context "MyRoseTree (inlineNonRecursiveSchemas)" $ checkInlinedRecSchema (Proxy :: Proxy MyRoseTree) myRoseTreeSchemaJSON
    context "MyRoseTree' (inlineNonRecursiveSchemas)" $ checkInlinedRecSchema (Proxy :: Proxy MyRoseTree') myRoseTreeSchemaJSON'
  describe "Bounded Enum key mapping" $ do
    context "ButtonImages" $ checkToSchema (Proxy :: Proxy ButtonImages) buttonImagesSchemaJSON
    context "TimeOfDay" $ checkToSchema (Proxy :: Proxy Data.Time.LocalTime.TimeOfDay) timeOfDaySchemaJSON

main :: IO ()
main = hspec spec
