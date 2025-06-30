{-# LANGUAGE CPP #-}

module Data.OpenApi.Aeson.Compat where

#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson        (Key)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KeyMap
#else
import qualified Data.HashMap.Strict as HM
#endif
import           Data.Bifunctor             (first)
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.Text                  as T

#if MIN_VERSION_aeson(2,0,0)
filterWithKey :: (T.Text -> v -> Bool) -> KeyMap.KeyMap v -> KeyMap.KeyMap v
filterWithKey p = KeyMap.filterWithKey (\k v -> p (keyToText k) v)

deleteKey :: Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
deleteKey = KeyMap.delete

objectToList :: KeyMap.KeyMap v -> [(Key, v)]
objectToList = KeyMap.toList

objectKeys :: KeyMap.KeyMap v -> [T.Text]
objectKeys = map Key.toText . KeyMap.keys

stringToKey :: String -> Key
stringToKey = Key.fromString

keyToString :: Key -> String
keyToString = Key.toString

keyToText :: Key -> T.Text
keyToText = Key.toText

toInsOrdHashMap :: KeyMap.KeyMap v -> InsOrdHashMap.InsOrdHashMap T.Text v
toInsOrdHashMap = InsOrdHashMap.fromList . fmap (first Key.toText) . KeyMap.toList

fromInsOrdHashMap :: InsOrdHashMap.InsOrdHashMap T.Text v -> KeyMap.KeyMap v
fromInsOrdHashMap = KeyMap.fromList . fmap (first Key.fromText) . InsOrdHashMap.toList

lookupKey :: T.Text -> KeyMap.KeyMap v -> Maybe v
lookupKey = KeyMap.lookup . Key.fromText

hasKey :: T.Text -> KeyMap.KeyMap a -> Bool
hasKey = KeyMap.member . Key.fromText

filterKeys :: (Key -> Bool) -> KeyMap.KeyMap a -> KeyMap.KeyMap a
filterKeys p = KeyMap.filterWithKey (\key _ -> p key)
#else
filterWithKey :: (T.Text -> v -> Bool) -> HM.HashMap T.Text v -> HM.HashMap T.Text v
filterWithKey = HM.filterWithKey

deleteKey :: T.Text -> HM.HashMap T.Text v -> HM.HashMap T.Text v
deleteKey = HM.delete

objectToList :: HM.HashMap T.Text v -> [(T.Text, v)]
objectToList = HM.toList

objectKeys :: HM.HashMap T.Text v -> [T.Text]
objectKeys = HM.keys

stringToKey :: String -> T.Text
stringToKey = T.pack

keyToString :: T.Text -> String
keyToString = T.unpack

keyToText :: T.Text -> T.Text
keyToText = id

toInsOrdHashMap :: HM.HashMap T.Text v -> InsOrdHashMap.InsOrdHashMap T.Text v
toInsOrdHashMap = InsOrdHashMap.fromHashMap

fromInsOrdHashMap :: InsOrdHashMap.InsOrdHashMap T.Text v -> HM.HashMap T.Text v
fromInsOrdHashMap = InsOrdHashMap.toHashMap

lookupKey :: T.Text -> HM.HashMap T.Text v -> Maybe v
lookupKey = HM.lookup

hasKey :: T.Text -> HM.HashMap T.Text a -> Bool
hasKey = HM.member

filterKeys :: (T.Text -> Bool) -> HM.HashMap T.Text a -> HM.HashMap T.Text a
filterKeys p = HM.filterWithKey (\key _ -> p key)
#endif
