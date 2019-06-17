{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Search.Result where

import Data.Aeson as Aeson hiding (Result)
import Data.Text (Text)
import GHC.Generics hiding (moduleName)



data Result =
  Result
  { _rType_ :: Type
  , _rPackageName :: Text
  , _rModuleName :: Text
  , _rValueName :: Text
  , _rValueComment :: Text
  , _rTypeSignature :: Text
  } deriving (Generic, Ord, Eq)


instance Aeson.ToJSON Result


data Type
  = Value
  | Package
  deriving (Generic, Ord, Eq, Show)


instance Aeson.ToJSON Type
