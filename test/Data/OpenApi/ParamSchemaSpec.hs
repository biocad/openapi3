{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.OpenApi.ParamSchemaSpec where

import Data.Aeson
import Data.Aeson.QQ.Simple
import Data.Char
import Data.Proxy
import GHC.Generics

import Data.OpenApi

import Data.OpenApi.CommonTestTypes
import SpecCommon
import Test.Hspec
import Data.Time.LocalTime

import qualified Data.HashMap.Strict as HM

checkToParamSchema :: ToParamSchema a => Proxy a -> Value -> Spec
checkToParamSchema proxy js = (toParamSchema proxy :: Schema) <=> js

spec :: Spec
spec = do
  describe "Generic ToParamSchema" $ do
    context "Unit" $ checkToParamSchema (Proxy :: Proxy Unit) unitSchemaJSON
    context "Color (bounded enum)" $ checkToParamSchema (Proxy :: Proxy Color) colorSchemaJSON
    context "Status (constructorTagModifier)" $ checkToParamSchema (Proxy :: Proxy Status) statusSchemaJSON
    context "Unary records" $ do
      context "Email (unary record)"  $ checkToParamSchema (Proxy :: Proxy Email)  emailSchemaJSON
      context "UserId (non-record newtype)" $ checkToParamSchema (Proxy :: Proxy UserId) userIdSchemaJSON
    context "TimeOfDay" $ checkToParamSchema (Proxy :: Proxy Data.Time.LocalTime.TimeOfDay) timeOfDayParamSchemaJSON

main :: IO ()
main = hspec spec
