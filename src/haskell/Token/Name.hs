{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Data.Ref as Ref



type Tokens =
  Map (Text, Int) [Ref.Ref]


-- VALUE NAMES


tokenizeValueNames :: [Package] -> Tokens
tokenizeValueNames =
  Map.fromListWith (++) . concatMap extractValueName


extractValueName :: Package -> [((Text, Int), [Ref.Ref])]
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
      ((typeName, 1), [Ref.valueRef package module_ typeName])
  in
    List.foldl toKeyValuePairs [] modules



-- MODULE NAMES


tokenizeModuleNames :: [Package] -> Tokens
tokenizeModuleNames =
  Map.fromListWith (++) . concatMap extractModuleName


extractModuleName :: Package -> [((Text, Int), [Ref.Ref])]
extractModuleName package@Package{modules} =
  let
    toKeyValuePairs module_@Module{moduleName} =
      ((moduleName, 1), [Ref.moduleRef package module_])
  in
    List.map toKeyValuePairs modules


-- PACKAGE NAMES


tokenizePackageNames :: [Package] -> Tokens
tokenizePackageNames =
  Map.fromListWith (++) . map extractPackageName


extractPackageName :: Package -> ((Text, Int), [Ref.Ref])
extractPackageName package@Package{packageName} =
  ((pn, 1), [Ref.packageRef package])
  where (_:pn:_) = Text.splitOn "/" packageName -- TODO: make total
