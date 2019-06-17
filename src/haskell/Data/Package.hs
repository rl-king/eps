{-# LANGUAGE OverloadedStrings #-}
module Data.Package where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Aeson as Aeson
import Data.Aeson.Types
import Data.Text (Text)



-- DEFINITIONS


type Name = Text
type Comment = Text
type Arguments = [Text]
type Type = Text


data Package = Package
  { _pName :: Name
  , _pSummary :: Text
  , _pVersions :: [Text]
  , _pModules :: [Module]
  } deriving (Show, Eq)


data Module = Module
  { _mName :: Name
  , _mComment :: Comment
  , _mDefs :: Map Name Def
  } deriving (Show, Eq)


data Def
  = TypeAlias Name Comment Arguments Type
  | CustomType Name Comment Arguments [(Text, [Type])]
  | Value_ Name Comment Type
  | Binop Name Comment Type
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
    Aeson.withObject "Module" $ \v -> do
      name <- v .: "name"
      comment <- v .: "comment"
      unions <- v .: "unions" >>= traverse parseUnion :: Parser [(Name, Def)]
      aliases <- v .: "aliases" >>= traverse parseAlias :: Parser [(Name, Def)]
      values <- v .: "values" >>= traverse parseValue :: Parser [(Name, Def)]
      binops <- v .: "binops" >>= traverse parseBinop :: Parser [(Name, Def)]
      return $ Module name comment (Map.fromList $ unions ++ aliases ++ values ++ binops)


parseAlias :: Value -> Parser (Name, Def)
parseAlias =
  Aeson.withObject "TypeAlias" $
  \v ->
    (\a b c d -> (a, TypeAlias a b c d))
    <$> v .: "name"
    <*> v .: "comment"
    <*> v .: "args"
    <*> v .: "type"


parseUnion :: Value -> Parser (Name, Def)
parseUnion =
  Aeson.withObject "CustomType" $
  \v ->
    (\a b c d -> (a, CustomType a b c d))
    <$> v .: "name"
    <*> v .: "comment"
    <*> v .: "args"
    <*> v .: "cases"


parseValue :: Value -> Parser (Name, Def)
parseValue =
    Aeson.withObject "Value_" $
    \v ->
      (\a b c  -> (a, Value_ a b c ))
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "type"


parseBinop :: Value -> Parser (Name, Def)
parseBinop =
    Aeson.withObject "Binop" $
    \v ->
      (\a b c -> (a, Binop a b c))
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
    toJSON (Module a b c) =
      object
      [ "name" .= a
      , "comment" .= b
      , "unions" .= c
      , "aliases" .= c
      , "values" .= c
      , "binops" .= c
      ]
    toEncoding (Module a b c) =
      pairs $
      "name" .= a <>
      "comment" .= b <>
      "unions" .= c <>
      "aliases" .= c <>
      "values" .= c <>
      "binops" .= c


instance Aeson.ToJSON Def where
    toJSON (TypeAlias a b c d) =
      object ["name" .= a, "comment" .= b, "args" .= c, "type" .= d]
    toJSON (CustomType a b c d) =
      object ["name" .= a, "comment" .= b, "args" .= c, "cases" .= d]
    toJSON (Value_ a b c) =
      object ["name" .= a, "comment" .= b, "type" .= c]
    toJSON (Binop a b c) =
      object ["name" .= a, "comment" .= b, "type" .= c]
    toEncoding (TypeAlias a b c d) =
      pairs ("name" .= a <> "comment" .= b <> "args" .= c <> "type" .= d)
    toEncoding (CustomType a b c d) =
      pairs ("name" .= a <> "comment" .= b <> "args" .= c <> "cases" .= d)
    toEncoding (Value_ a b c) =
      pairs ("name" .= a <> "comment" .= b <> "type" .= c)
    toEncoding (Binop a b c) =
      pairs ("name" .= a <> "comment" .= b <> "type" .= c)
