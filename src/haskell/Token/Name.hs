{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name
  ( Tokens
  , Token
  , query
  , tokenizePackageNames
  , tokenizeModuleNames
  , tokenizeValueNames
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Data.Ref as Ref



-- DEFINITIONS


newtype Tokens =
  Tokens { tokens :: Map Token [Ref.Ref] }
  deriving (Show)


newtype Token =
  Token { token :: Text }
  deriving (Eq, Ord, Show)



-- QUERY


query :: Text -> Tokens -> [Ref.Ref]
query term (Tokens idx) =
  []



-- PACKAGE NAMES


tokenizePackageNames :: [Package] -> Tokens
tokenizePackageNames =
  Tokens . Map.fromListWith (++) . map extractPackageName


extractPackageName :: Package -> (Token, [Ref.Ref])
extractPackageName package@Package{packageName} =
  case Text.splitOn "/" packageName of
    _:name:_ ->
      (Token name, [Ref.packageRef package])

    _ ->
      (Token packageName, [Ref.packageRef package]) -- Kinda weird, maybe filtermap instead



-- MODULE NAMES


tokenizeModuleNames :: [Package] -> Tokens
tokenizeModuleNames =
  Tokens . Map.fromListWith (++) . concatMap extractModuleName


extractModuleName :: Package -> [(Token, [Ref.Ref])]
extractModuleName package@Package{modules} =
  let
    toKeyValuePairs module_@Module{moduleName} =
      (Token moduleName, [Ref.moduleRef package module_])
  in
    List.map toKeyValuePairs modules



-- VALUE NAMES


tokenizeValueNames :: [Package] -> Tokens
tokenizeValueNames =
  Tokens . Map.fromListWith (++) . concatMap extractValueName


extractValueName :: Package -> [(Token, [Ref.Ref])]
extractValueName package@Package{modules} =
  let
    toKeyValuePairs acc module_@Module{values, binops, aliases} =
      List.map (valueToPair module_) values ++
      List.map (aliasesToPair module_) aliases ++
      List.map (binopToPair module_) binops ++
      acc

    aliasesToPair module_ (TypeAlias typeName _ _ _) =
      toPair module_ typeName

    binopToPair module_ (Binop typeName _ _) =
      toPair module_ typeName

    valueToPair module_ (Value_ typeName _ _) =
      toPair module_ typeName

    toPair module_ typeName =
      (Token typeName, [Ref.valueRef package module_ typeName])
  in
    List.foldl toKeyValuePairs [] modules
