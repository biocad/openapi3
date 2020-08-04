{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics

import Data.OpenApi
import Data.OpenApi.Declare
import Data.OpenApi.Lens
import Data.OpenApi.Operation

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Generic)

instance ToSchema UserSummary where
  declareNamedSchema _ = do
    usernameSchema <- declareSchemaRef (Proxy :: Proxy Username)
    useridSchema   <- declareSchemaRef (Proxy :: Proxy Int)
    return $ NamedSchema (Just "UserSummary") $ mempty
      & type_ ?~ OpenApiObject
      & properties .~
          [ ("summaryUsername", usernameSchema )
          , ("summaryUserid"  , useridSchema   )
          ]
      & required .~ [ "summaryUsername"
                    , "summaryUserid"   ]


type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Generic, ToSchema)

newtype Package = Package { packageName :: Text }
  deriving (Generic, ToSchema)

hackageOpenApi :: OpenApi
hackageOpenApi = spec & components.schemas .~ defs
  where
    (defs, spec) = runDeclare declareHackageOpenApi mempty

declareHackageOpenApi :: Declare (Definitions Schema) OpenApi
declareHackageOpenApi = do
  -- param schemas
  let usernameParamSchema = toParamSchema (Proxy :: Proxy Username)

  -- responses
  userSummaryResponse   <- declareResponse "application/json" (Proxy :: Proxy UserSummary)
  userDetailedResponse  <- declareResponse "application/json" (Proxy :: Proxy UserDetailed)
  packagesResponse      <- declareResponse "application/json" (Proxy :: Proxy [Package])

  return $ mempty
    & paths .~
        [ ("/users", mempty & get ?~ (mempty
            & at 200 ?~ Inline userSummaryResponse))
        , ("/user/{username}", mempty & get ?~ (mempty
            & parameters .~ [ Inline $ mempty
                & name .~ "username"
                & required ?~ True
                & in_ .~ ParamPath
                & schema ?~ Inline usernameParamSchema ]
            & at 200 ?~ Inline userDetailedResponse))
        , ("/packages", mempty & get ?~ (mempty
            & at 200 ?~ Inline packagesResponse))
        ]

main :: IO ()
main = putStrLn . read . show . encode $ hackageOpenApi

