{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Search.Result where

import Data.Aeson as Aeson hiding (Result)
import Data.Text (Text)
import GHC.Generics hiding (moduleName)
import qualified Data.Package



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


fromPackage :: Data.Package.Package -> Result
fromPackage package =
  Result Package (Data.Package.packageName package) "" "" "" ""
