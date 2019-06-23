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
  , _pModules :: Map Name Module
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


comment :: Def -> Text
comment def = case defInfo def of (_,c,_,_,_) -> c


typeSig :: Def -> Text
typeSig def = case defInfo def of (_,_,_,t,_) -> t


category :: Def -> Text
category def = case defInfo def of (_,_,_,_,c) -> c


defInfo :: Def -> (Text, Text, Text, Text, Text)
defInfo def =
  case def of
    TypeAlias n c _ t -> (n, c, "", t, "Type alias")
    CustomType n c _ _ -> (n, c, "", "", "Custom type")
    Value_ n c t -> (n, c, "", t, "Expression")
    Binop n c t -> (n, c, "", t, "Binary operator")


-- DECODERS


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $
    \v ->
      Package
      <$> v .: "name"
      <*> v .: "summary"
      <*> v .: "versions"
      <*> pure Map.empty


instance Aeson.FromJSON Module where
  parseJSON =
    Aeson.withObject "Module" $ \v -> do
      name <- v .: "name"
      comment_ <- v .: "comment"
      unions <- v .: "unions" >>= traverse parseUnion
      aliases <- v .: "aliases" >>= traverse parseAlias
      values <- v .: "values" >>= traverse parseValue
      binops <- v .: "binops" >>= traverse parseBinop
      return $ Module name comment_
        (Map.fromList $ unions ++ aliases ++ values ++ binops)


parseUnion :: Value -> Parser (Name, Def)
parseUnion =
  Aeson.withObject "Union" $
  \v ->
    (\n c a b -> (n, CustomType n c a b))
    <$> v .: "name"
    <*> v .: "comment"
    <*> v .: "args"
    <*> v .: "cases"


parseAlias :: Value -> Parser (Name, Def)
parseAlias =
  Aeson.withObject "TypeAlias" $
  \v ->
    (\a b c d -> (a, TypeAlias a b c d))
    <$> v .: "name"
    <*> v .: "comment"
    <*> v .: "args"
    <*> v .: "type"


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
