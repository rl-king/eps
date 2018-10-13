{-# LANGUAGE OverloadedStrings #-}
module Data.Package where

import Data.Aeson as Aeson
import Data.Text (Text)



-- DEFINITIONS


type Name = Text
type Comment = Text
type Arguments = [Text]
type Type = Text


data Package = Package
  { packageName :: Name
  , summary :: Text
  , versions :: [Text]
  , modules :: [Module]
  } deriving (Show, Eq)


data Module = Module
  { moduleName :: Name
  , comment :: Comment
  , customTypes :: [CustomType]
  , aliases :: [TypeAlias]
  , values :: [Value_]
  , binops :: [Binop]
  } deriving (Show, Eq)


data TypeAlias =
  TypeAlias Name Comment Arguments Type
  deriving (Show, Eq)


data CustomType =
  CustomType Name Comment Arguments [(Text, [Type])]
  deriving (Show, Eq)


data Value_ =
  Value_ Name Comment Type
  deriving (Show, Eq)


data Binop =
  Binop Name Comment Type
  deriving (Show, Eq)


-- DECODERS


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $
    \v ->
      Package
      <$> v .: "name"
      <*> v .: "summary"
      <*> v .: "versions"
      <*> v .:? "docs" .!= [] -- Fallback to [] as this is not in search.json


instance Aeson.FromJSON Module where
  parseJSON =
    Aeson.withObject "Module" $
    \v ->
      Module
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "unions"
      <*> v .: "aliases"
      <*> v .: "values"
      <*> v .: "binops"


instance Aeson.FromJSON TypeAlias where
  parseJSON =
    Aeson.withObject "TypeAlias" $
    \v ->
      TypeAlias
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "args"
      <*> v .: "type"


instance Aeson.FromJSON CustomType where
  parseJSON =
    Aeson.withObject "CustomType" $
    \v ->
      CustomType
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "args"
      <*> v .: "cases"


instance Aeson.FromJSON Value_ where
  parseJSON =
    Aeson.withObject "Value_" $
    \v ->
      Value_
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "type"


instance Aeson.FromJSON Binop where
  parseJSON =
    Aeson.withObject "Binop" $
    \v ->
      Binop
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "type"


-- ENCODERS


instance Aeson.ToJSON Package where
    toJSON (Package x y z d) =
      object ["name" .= x, "summary" .= y, "versions" .= z, "docs" .= d]
    toEncoding (Package x y z d) =
      pairs ("name" .= x <> "summary" .= y <> "versions" .= z <> "docs" .= d)


instance Aeson.ToJSON Module where
    toJSON (Module a b c d e f) =
      object
      [ "name" .= a
      , "comment" .= b
      , "unions" .= c
      , "aliases" .= d
      , "values" .= e
      , "binops" .= f
      ]
    toEncoding (Module a b c d e f) =
      pairs $
      "name" .= a <>
      "comment" .= b <>
      "unions" .= c <>
      "aliases" .= d <>
      "values" .= e <>
      "binops" .= f


instance Aeson.ToJSON TypeAlias where
    toJSON (TypeAlias a b c d) =
      object ["name" .= a, "comment" .= b, "args" .= c, "type" .= d]
    toEncoding (TypeAlias a b c d) =
      pairs ("name" .= a <> "comment" .= b <> "args" .= c <> "type" .= d)


instance Aeson.ToJSON CustomType where
    toJSON (CustomType a b c d) =
      object ["name" .= a, "comment" .= b, "args" .= c, "cases" .= d]
    toEncoding (CustomType a b c d) =
      pairs ("name" .= a <> "comment" .= b <> "args" .= c <> "cases" .= d)


instance Aeson.ToJSON Value_ where
    toJSON (Value_ a b c) =
      object ["name" .= a, "comment" .= b, "type" .= c]
    toEncoding (Value_ a b c) =
      pairs ("name" .= a <> "comment" .= b <> "type" .= c)


instance Aeson.ToJSON Binop where
    toJSON (Binop a b c) =
      object ["name" .= a, "comment" .= b, "type" .= c]
    toEncoding (Binop a b c) =
      pairs ("name" .= a <> "comment" .= b <> "type" .= c)
