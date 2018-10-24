{-# LANGUAGE NamedFieldPuns #-}
module Token.ValueName where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.Result as SR



type Tokens =
  Map (Text, Int) [SR.Result]


tokenize :: [Package] -> Tokens
tokenize =
  Map.fromListWith (++) . concatMap extract


extract :: Package -> [((Text, Int), [SR.Result])]
extract Package{packageName, modules} =
  let
    toKeyValuePairs acc Module{moduleName, values} =
      List.map (toKeyValuePairsHelper moduleName) values ++ acc

    toKeyValuePairsHelper moduleName (Value_ typeName comment type_) =
      ((typeName, 1) ,[SR.Result SR.Value packageName moduleName typeName comment type_])
  in
    List.foldl toKeyValuePairs [] modules
