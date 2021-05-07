{-# OPTIONS_GHC -Wall                  #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Module:      Data.OpenApi.Internal.Schema.Validation
-- Copyright:   (c) 2015 GetShopTV
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Validate JSON values with Swagger Schema.
module Data.OpenApi.Internal.Schema.Validation where

import           Prelude                             ()
import           Prelude.Compat

import           Control.Applicative
import           Control.Lens                        hiding (allOf)
import           Control.Monad                       (forM, when)

import           Data.Aeson                          hiding (Result)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import           Data.Foldable                       (for_, sequenceA_,
                                                      traverse_)
#if !MIN_VERSION_aeson(2,0,0)
import           Data.HashMap.Strict                 (HashMap)
#endif
import qualified Data.HashMap.Strict.InsOrd          as InsOrdHashMap
import qualified "unordered-containers" Data.HashSet as HashSet
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy
import           Data.Scientific                     (Scientific, isInteger)
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Lazy                      as TL
import qualified Data.Text.Lazy.Encoding             as TL
import           Data.Traversable                    (for)
import           Data.Vector                         (Vector)
import qualified Data.Vector                         as Vector

import Data.OpenApi.Aeson.Compat    (hasKey, keyToText, lookupKey, objectToList)
import Data.OpenApi.Declare
import Data.OpenApi.Internal
import Data.OpenApi.Internal.Schema
import Data.OpenApi.Internal.Utils
import Data.OpenApi.Lens

-- | Validate @'ToJSON'@ instance matches @'ToSchema'@ for a given value.
-- This can be used with QuickCheck to ensure those instances are coherent:
--
-- prop> validateToJSON (x :: Int) == []
--
-- /NOTE:/ @'validateToJSON'@ does not perform string pattern validation.
-- See @'validateToJSONWithPatternChecker'@.
--
-- See 'renderValidationErrors' on how the output is structured.
validatePrettyToJSON :: forall a. (ToJSON a, ToSchema a) => a -> Maybe String
validatePrettyToJSON = renderValidationErrors validateToJSON

-- | Variant of 'validatePrettyToJSON' with typed output.
validateToJSON :: forall a. (ToJSON a, ToSchema a) => a -> [ValidationError]
validateToJSON = validateToJSONWithPatternChecker (\_pattern _str -> True)

-- | Validate @'ToJSON'@ instance matches @'ToSchema'@ for a given value and pattern checker.
-- This can be used with QuickCheck to ensure those instances are coherent.
--
-- For validation without patterns see @'validateToJSON'@.  See also:
-- 'renderValidationErrors'.
validateToJSONWithPatternChecker :: forall a. (ToJSON a, ToSchema a) => (Pattern -> Text -> Bool) -> a -> [ValidationError]
validateToJSONWithPatternChecker checker = validateJSONWithPatternChecker checker defs sch . toJSON
  where
    (defs, sch) = runDeclare (declareSchema (Proxy :: Proxy a)) mempty

-- | Pretty print validation errors
-- together with actual JSON and Swagger Schema
-- (using 'encodePretty').
--
-- >>> import Data.Aeson as Aeson
-- >>> import Data.Foldable (traverse_)
-- >>> import GHC.Generics
-- >>> data Phone = Phone { value :: String } deriving (Generic)
-- >>> data Person = Person { name :: String, phone :: Phone } deriving (Generic)
-- >>> instance ToJSON Person where toJSON p = object [ "name" Aeson..= name p ]
-- >>> instance ToSchema Phone
-- >>> instance ToSchema Person
-- >>> let person = Person { name = "John", phone = Phone "123456" }
-- >>> traverse_ putStrLn $ renderValidationErrors validateToJSON person
-- Validation against the schema fails:
--   * property "phone" is required, but not found in "{\"name\":\"John\"}"
-- <BLANKLINE>
-- JSON value:
-- {
--     "name": "John"
-- }
-- <BLANKLINE>
-- Swagger Schema:
-- {
--     "properties": {
--         "name": {
--             "type": "string"
--         },
--         "phone": {
--             "$ref": "#/components/schemas/Phone"
--         }
--     },
--     "required": [
--         "name",
--         "phone"
--     ],
--     "type": "object"
-- }
-- <BLANKLINE>
-- Swagger Description Context:
-- {
--     "Phone": {
--         "properties": {
--             "value": {
--                 "type": "string"
--             }
--         },
--         "required": [
--             "value"
--         ],
--         "type": "object"
--     }
-- }
-- <BLANKLINE>
renderValidationErrors
  :: forall a. (ToJSON a, ToSchema a)
  => (a -> [ValidationError]) -> a -> Maybe String
renderValidationErrors f x =
  case f x of
    []      -> Nothing
    errors  -> Just $ unlines
      [ "Validation against the schema fails:"
      , unlines (map ("  * " ++) errors)
      , "JSON value:"
      , ppJSONString (toJSON x)
      , ""
      , "Swagger Schema:"
      , ppJSONString (toJSON schema_)
      , ""
      , "Swagger Description Context:"
      , ppJSONString (toJSON refs_)
      ]
  where
    ppJSONString = TL.unpack . TL.decodeUtf8 . encodePretty
    (refs_, schema_) = runDeclare (declareSchema (Proxy :: Proxy a)) mempty

-- | Validate JSON @'Value'@ against Swagger @'Schema'@.
--
-- prop> validateJSON mempty (toSchema (Proxy :: Proxy Int)) (toJSON (x :: Int)) == []
--
-- /NOTE:/ @'validateJSON'@ does not perform string pattern validation.
-- See @'validateJSONWithPatternChecker'@.
validateJSON :: Definitions Schema -> Schema -> Value -> [ValidationError]
validateJSON = validateJSONWithPatternChecker (\_pattern _str -> True)

-- | Validate JSON @'Value'@ agains Swagger @'ToSchema'@ for a given value and pattern checker.
--
-- For validation without patterns see @'validateJSON'@.
validateJSONWithPatternChecker :: (Pattern -> Text -> Bool) -> Definitions Schema -> Schema -> Value -> [ValidationError]
validateJSONWithPatternChecker checker defs sch js =
  case runValidation (validateWithSchema js) cfg sch of
    Failed xs -> xs
    Passed _  -> mempty
  where
    cfg = defaultConfig
            { configPatternChecker = checker
            , configDefinitions = defs }

-- | Validation error message.
type ValidationError = String

-- | Validation result type.
data Result a
  = Failed [ValidationError]  -- ^ Validation failed with a list of error messages.
  | Passed a                  -- ^ Validation passed.
  deriving (Eq, Show, Functor)

instance Applicative Result where
  pure = Passed
  Passed f <*> Passed x = Passed (f x)
  Failed xs <*> Failed ys = Failed (xs <> ys)
  Failed xs <*> _ = Failed xs
  _ <*> Failed ys = Failed ys

instance Alternative Result where
  empty = Failed mempty
  Passed x <|> _ = Passed x
  _        <|> y = y

instance Monad Result where
  return = pure
  Passed x >>=  f = f x
  Failed xs >>= _ = Failed xs

-- | Validation configuration.
data Config = Config
  { -- | Pattern checker for @'_schemaPattern'@ validation.
    configPatternChecker :: Pattern -> Text -> Bool
    -- | Schema definitions in scope to resolve references.
  , configDefinitions    :: Definitions Schema
  }

-- | Default @'Config'@:
--
-- @
-- defaultConfig = 'Config'
--   { 'configPatternChecker' = \\_pattern _str -> True
--   , 'configDefinitions'    = mempty
--   }
-- @
defaultConfig :: Config
defaultConfig = Config
  { configPatternChecker = \_pattern _str -> True
  , configDefinitions    = mempty
  }

-- | Value validation.
newtype Validation s a = Validation { runValidation :: Config -> s -> Result a }
  deriving (Functor)

instance Applicative (Validation schema) where
  pure x = Validation (\_ _ -> pure x)
  Validation f <*> Validation x = Validation (\c s -> f c s <*> x c s)

instance Alternative (Validation schema) where
  empty = Validation (\_ _ -> empty)
  Validation x <|> Validation y = Validation (\c s -> x c s <|> y c s)

instance Profunctor Validation where
  dimap f g (Validation k) = Validation (\c s -> fmap g (k c (f s)))

instance Choice Validation where
  left'  (Validation g) = Validation (\c -> either (fmap Left . g c) (pure . Right))
  right' (Validation g) = Validation (\c -> either (pure . Left) (fmap Right . g c))

instance Monad (Validation s) where
  return = pure
  Validation x >>= f = Validation (\c s -> x c s >>= \y -> runValidation (f y) c s)
  (>>) = (*>)

withConfig :: (Config -> Validation s a) -> Validation s a
withConfig f = Validation (\c -> runValidation (f c) c)

withSchema :: (s -> Validation s a) -> Validation s a
withSchema f = Validation (\c s -> runValidation (f s) c s)

-- | Issue an error message.
invalid :: String -> Validation schema a
invalid msg = Validation (\_ _ -> Failed [msg])

-- | Validation passed.
valid :: Validation schema ()
valid = pure ()

-- | Validate schema's property given a lens into that property
-- and property checker.
checkMissing :: Validation s () -> Lens' s (Maybe a) -> (a -> Validation s ()) -> Validation s ()
checkMissing missing l g = withSchema $ \sch ->
  case sch ^. l of
    Nothing -> missing
    Just x  -> g x

-- | Validate schema's property given a lens into that property
-- and property checker.
-- If property is missing in schema, consider it valid.
check :: Lens' s (Maybe a) -> (a -> Validation s ()) -> Validation s ()
check = checkMissing valid

-- | Validate same value with different schema.
sub :: t -> Validation t a -> Validation s a
sub = lmap . const

-- | Validate same value with a part of the original schema.
sub_ :: Getting a s a -> Validation a r -> Validation s r
sub_ = lmap . view

-- | Validate value against a schema given schema reference and validation function.
withRef :: Reference -> (Schema -> Validation s a) -> Validation s a
withRef (Reference ref) f = withConfig $ \cfg ->
  case InsOrdHashMap.lookup ref (configDefinitions cfg) of
    Nothing -> invalid $ "unknown schema " ++ show ref
    Just s  -> f s

validateWithSchemaRef :: Referenced Schema -> Value -> Validation s ()
validateWithSchemaRef (Ref ref)  js = withRef ref $ \sch -> sub sch (validateWithSchema js)
validateWithSchemaRef (Inline s) js = sub s (validateWithSchema js)

-- | Validate JSON @'Value'@ with Swagger @'Schema'@.
validateWithSchema :: Value -> Validation Schema ()
validateWithSchema val = do
  validateSchemaType val
  validateEnum val

validateInteger :: Scientific -> Validation Schema ()
validateInteger n = do
  when (not (isInteger n)) $
    invalid ("not an integer")
  validateNumber n

validateNumber :: Scientific -> Validation Schema ()
validateNumber n = withConfig $ \_cfg -> withSchema $ \sch -> do
  let exMax = Just True == sch ^. exclusiveMaximum
      exMin = Just True == sch ^. exclusiveMinimum

  check maximum_ $ \m ->
    when (if exMax then (n >= m) else (n > m)) $
      invalid ("value " ++ show n ++ " exceeds maximum (should be " ++ (if exMax then "<" else "<=") ++ show m ++ ")")

  check minimum_ $ \m ->
    when (if exMin then (n <= m) else (n < m)) $
      invalid ("value " ++ show n ++ " falls below minimum (should be " ++ (if exMin then ">" else ">=") ++ show m ++ ")")

  check multipleOf $ \k ->
    when (not (isInteger (n / k))) $
      invalid ("expected a multiple of " ++ show k ++ " but got " ++ show n)

validateString :: Text -> Validation Schema ()
validateString s = do
  check maxLength $ \n ->
    when (len > fromInteger n) $
      invalid ("string is too long (length should be <=" ++ show n ++ ")")

  check minLength $ \n ->
    when (len < fromInteger n) $
      invalid ("string is too short (length should be >=" ++ show n ++ ")")

  check pattern $ \regex -> do
    withConfig $ \cfg -> do
      when (not (configPatternChecker cfg regex s)) $
        invalid ("string does not match pattern " ++ show regex)
  where
    len = Text.length s

validateArray :: Vector Value -> Validation Schema ()
validateArray xs = do
  check maxItems $ \n ->
    when (len > fromInteger n) $
      invalid ("array exceeds maximum size (should be <=" ++ show n ++ ")")

  check minItems $ \n ->
    when (len < fromInteger n) $
      invalid ("array is too short (size should be >=" ++ show n ++ ")")

  check items $ \case
    OpenApiItemsObject itemSchema      -> traverse_ (validateWithSchemaRef itemSchema) xs
    OpenApiItemsArray itemSchemas -> do
      when (len /= length itemSchemas) $
        invalid ("array size is invalid (should be exactly " ++ show (length itemSchemas) ++ ")")
      sequenceA_ (zipWith validateWithSchemaRef itemSchemas (Vector.toList xs))

  check uniqueItems $ \unique ->
    when (unique && not allUnique) $
      invalid ("array is expected to contain unique items, but it does not")
  where
    len = Vector.length xs
    allUnique = len == HashSet.size (HashSet.fromList (Vector.toList xs))

validateObject ::
#if MIN_VERSION_aeson(2,0,0)
  KeyMap.KeyMap Value
#else
  HashMap Text Value
#endif
  -> Validation Schema ()
validateObject o = withSchema $ \sch ->
  case sch ^. discriminator of
    Just (Discriminator pname types) -> case fromJSON <$> lookupKey pname o of
      Just (Success pvalue) ->
        let ref = fromMaybe pvalue $ InsOrdHashMap.lookup pvalue types
        -- TODO ref may be name or reference
        in validateWithSchemaRef (Ref (Reference ref)) (Object o)
      Just (Error msg)   -> invalid ("failed to parse discriminator property " ++ show pname ++ ": " ++ show msg)
      Nothing            -> invalid ("discriminator property " ++ show pname ++ "is missing")
    Nothing -> do
      check maxProperties $ \n ->
        when (size > n) $
          invalid ("object size exceeds maximum (total number of properties should be <=" ++ show n ++ ")")

      check minProperties $ \n ->
        when (size < n) $
          invalid ("object size is too small (total number of properties should be >=" ++ show n ++ ")")

      validateRequired
      validateProps
  where
    size = fromIntegral (length o)

    validateRequired = withSchema $ \sch -> traverse_ validateReq (sch ^. required)
    validateReq n =
      when (not (hasKey n o)) $
        invalid ("property " ++ show n ++ " is required, but not found in " ++ show (encode o))

    validateProps = withSchema $ \sch -> do
      for_ (objectToList o) $ \(keyToText -> k, v) ->
        case v of
          Null | not (k `elem` (sch ^. required)) -> valid  -- null is fine for non-required property
          _ ->
            case InsOrdHashMap.lookup k (sch ^. properties) of
              Nothing -> checkMissing (unknownProperty k) additionalProperties $ validateAdditional k v
              Just s  -> validateWithSchemaRef s v

    validateAdditional _ _ (AdditionalPropertiesAllowed True) = valid
    validateAdditional k _ (AdditionalPropertiesAllowed False) = invalid $ "additionalProperties=false but extra property " <> show k <> " found"
    validateAdditional _ v (AdditionalPropertiesSchema s) = validateWithSchemaRef s v

    unknownProperty :: Text -> Validation s a
    unknownProperty pname = invalid $
      "property " <> show pname <> " is found in JSON value, but it is not mentioned in Swagger schema"

validateEnum :: Value -> Validation Schema ()
validateEnum val = do
  check enum_ $ \xs ->
    when (val `notElem` xs) $
      invalid ("expected one of " ++ show (encode xs) ++ " but got " ++ show val)

-- | Infer schema type based on used properties.
--
-- This is like 'inferParamSchemaTypes', but also works for objects:
--
-- >>> inferSchemaTypes <$> decode "{\"minProperties\": 1}"
-- Just [OpenApiObject]
inferSchemaTypes :: Schema -> [OpenApiType]
inferSchemaTypes sch = inferParamSchemaTypes sch ++
  [ OpenApiObject | any ($ sch)
       [ has (additionalProperties._Just)
       , has (maxProperties._Just)
       , has (minProperties._Just)
       , has (properties.folded)
       , has (required.folded) ] ]

-- | Infer schema type based on used properties.
--
-- >>> inferSchemaTypes <$> decode "{\"minLength\": 2}"
-- Just [OpenApiString]
--
-- >>> inferSchemaTypes <$> decode "{\"maxItems\": 0}"
-- Just [OpenApiArray]
--
-- From numeric properties 'OpenApiInteger' type is inferred.
-- If you want 'OpenApiNumber' instead, you must specify it explicitly.
--
-- >>> inferSchemaTypes <$> decode "{\"minimum\": 1}"
-- Just [OpenApiInteger]
inferParamSchemaTypes :: Schema -> [OpenApiType]
inferParamSchemaTypes sch = concat
  [ [ OpenApiArray | any ($ sch)
        [ has (items._Just)
        , has (maxItems._Just)
        , has (minItems._Just)
        , has (uniqueItems._Just) ] ]
  , [ OpenApiInteger | any ($ sch)
        [ has (exclusiveMaximum._Just)
        , has (exclusiveMinimum._Just)
        , has (maximum_._Just)
        , has (minimum_._Just)
        , has (multipleOf._Just) ] ]
  , [ OpenApiString | any ($ sch)
        [ has (maxLength._Just)
        , has (minLength._Just)
        , has (pattern._Just) ] ]
  ]

validateSchemaType :: Value -> Validation Schema ()
validateSchemaType val = withSchema $ \sch ->
  case sch of
    (view oneOf -> Just variants) -> do
      res <- forM variants $ \var ->
        (True <$ validateWithSchemaRef var val) <|> (return False)
      case length $ filter id res of
        0 -> invalid $ "Value not valid under any of 'oneOf' schemas: " ++ show val
        1 -> valid
        _ -> invalid $ "Value matches more than one of 'oneOf' schemas: " ++ show val
    (view allOf -> Just variants) -> do
      schemas <- for variants $ \case
        Ref ref  -> withRef ref pure
        Inline s -> pure s

      sub (mconcat schemas) $ validateWithSchema val

    _ ->
      case (sch ^. type_, val) of
        (Just OpenApiNull,    Null)       -> valid
        (Just OpenApiBoolean, Bool _)     -> valid
        (Just OpenApiInteger, Number n)   -> validateInteger n
        (Just OpenApiNumber,  Number n)   -> validateNumber n
        (Just OpenApiString,  String s)   -> validateString s
        (Just OpenApiArray,   Array xs)   -> validateArray xs
        (Just OpenApiObject,  Object o)   -> validateObject o
        (Nothing, Null)                   -> valid
        (Nothing, Bool _)                 -> valid
        -- Number by default
        (Nothing, Number n)               -> validateNumber n
        (Nothing, String s)               -> validateString s
        (Nothing, Array xs)               -> validateArray xs
        (Nothing, Object o)               -> validateObject o
        bad -> invalid $ "expected JSON value of type " ++ showType bad

validateParamSchemaType :: Value -> Validation Schema ()
validateParamSchemaType val = withSchema $ \sch ->
  case (sch ^. type_, val) of
    (Just OpenApiBoolean, Bool _)     -> valid
    (Just OpenApiInteger, Number n)   -> validateInteger n
    (Just OpenApiNumber,  Number n)   -> validateNumber n
    (Just OpenApiString,  String s)   -> validateString s
    (Just OpenApiArray,   Array xs)   -> validateArray xs
    (Nothing, Bool _)                 -> valid
    -- Number by default
    (Nothing, Number n)               -> validateNumber n
    (Nothing, String s)               -> validateString s
    (Nothing, Array xs)               -> validateArray xs
    bad -> invalid $ "expected JSON value of type " ++ showType bad

showType :: (Maybe OpenApiType, Value) -> String
showType (Just ty, _)        = show ty
showType (Nothing, Null)     = "OpenApiNull"
showType (Nothing, Bool _)   = "OpenApiBoolean"
showType (Nothing, Number _) = "OpenApiNumber"
showType (Nothing, String _) = "OpenApiString"
showType (Nothing, Array _)  = "OpenApiArray"
showType (Nothing, Object _) = "OpenApiObject"
