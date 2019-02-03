{-# LANGUAGE NamedFieldPuns #-}
module Token.Name where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.Result as SR



type Tokens =
  Map (Text, Int) [SR.Result]


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
