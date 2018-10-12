{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Token.Value where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text, words, filter, splitOn)
import Data.Map.Strict (Map)

import Data.Package as Package


type ValueTokens =
  Map Text [ValueInfo]

data ValueInfo =
  ValueInfo
  { packageName_ :: Text
  , moduleName_ :: Text
  , typeName :: Text
  , typeSignature :: Text
  }


tokenize :: [Package] -> Map Text [ValueInfo]
tokenize packages =
  let
    extract (Package{packageName, modules}) =
      extractType packageName modules
  in
  Map.fromListWith (++) $ concat $ fmap extract packages


extractType :: Text -> [Module] -> [(Text, [ValueInfo])]
extractType packageName modules =
  let
    listTypes acc Module{moduleName, values} =
      (++) acc $ concat $ List.map (fromValues moduleName) values

    fromValues moduleName (Value_ typeName _ type_) =
      List.map (\x -> (removeModule x , [ValueInfo packageName moduleName typeName type_])) $
      Data.Text.words $
      removeChars type_

    removeModule t =
      last $ Data.Text.splitOn "." t

    removeChars t =
      Data.Text.filter (not . flip elem ['(', ')', '-', '>']) t
  in
    List.foldl listTypes [] modules
