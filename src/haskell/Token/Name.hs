{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.Result as SR



type Tokens =
  Map (Text, Int) [SR.Result]


-- VALUE NAMES


tokenizeValueNames :: [Package] -> Tokens
tokenizeValueNames =
  Map.fromListWith (++) . concatMap extractValueName


extractValueName :: Package -> [((Text, Int), [SR.Result])]
extractValueName Package{packageName, modules} =
  let
    toKeyValuePairs acc Module{moduleName, values, binops, aliases} =
      List.map (valueToPair moduleName) values ++
      List.map (aliasesToPair moduleName) aliases ++
      List.map (binopToPair moduleName) binops ++
      acc

    aliasesToPair moduleName (TypeAlias typeName comment _ type_) =
      toPair moduleName typeName comment type_

    binopToPair moduleName (Binop typeName comment type_) =
      toPair moduleName typeName comment type_

    valueToPair moduleName (Value_ typeName comment type_) =
      toPair moduleName typeName comment type_

    toPair moduleName typeName comment type_ =
      ((typeName, 1), [SR.Result SR.Value packageName moduleName typeName comment type_])
  in
    List.foldl toKeyValuePairs [] modules



-- MODULE NAMES


tokenizeModuleNames :: [Package] -> Tokens
tokenizeModuleNames =
  Map.fromListWith (++) . concatMap extractModuleName


extractModuleName :: Package -> [((Text, Int), [SR.Result])]
extractModuleName Package{packageName, modules} =
  let
    toKeyValuePairs Module{moduleName} =
      ((moduleName, 1), [SR.Result SR.Value packageName moduleName "" "" ""])
  in
    List.map toKeyValuePairs modules


-- PACKAGE NAMES


tokenizePackageNames :: [Package] -> Tokens
tokenizePackageNames =
  Map.fromListWith (++) . map extractPackageName


extractPackageName :: Package -> ((Text, Int), [SR.Result])
extractPackageName Package{packageName} =
  ((pn, 1), [SR.Result SR.Value pn "" "" "" ""])
  where (_:pn:_) = Text.splitOn "/" packageName -- TODO: make total
