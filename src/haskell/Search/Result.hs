{-# LANGUAGE DeriveGeneric #-}
module Search.Result where

import Data.Aeson as Aeson hiding (Result)
import Data.Text (Text)
import GHC.Generics



data Result =
  Result
  { category :: Category
  , packageName :: Text
  , moduleName :: Text
  , valueName :: Text
  , valueComment :: Text
  , typeSignature :: Text
  } deriving (Generic, Ord, Eq)


instance Aeson.ToJSON Result


data Category =
  Value | Package
  deriving (Generic, Ord, Eq, Show)


instance Aeson.ToJSON Category
