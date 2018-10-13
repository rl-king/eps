{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Token.Value where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Data.Text (Text, head, words, filter, splitOn, uncons, singleton)
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
  } deriving (Show)


tokenize :: [Package] -> Map Text [ValueInfo]
tokenize packages =
  let
    extract (Package{packageName, modules}) =
      extractNamedType packageName modules
  in
    Map.fromListWith (++) $ concat $ fmap extract packages


extractNamedType :: Text -> [Module] -> [(Text, [ValueInfo])]
extractNamedType packageName modules =
  let
    listTypes acc Module{moduleName, values} =
      (++) acc $ concat $ List.map (fromValues moduleName) values

    fromValues moduleName (Value_ typeName _ type_) =
      List.map (\x -> (removeModule x , [ValueInfo packageName moduleName typeName type_])) $
      (\(_,_,x) -> x) $
      List.foldr simplifyTypeParams (Map.empty, ['a'..], []) $
      Data.Text.words $
      removeChars type_

    removeModule t =
      last $ Data.Text.splitOn "." t

    removeChars t =
      Data.Text.filter (not . flip elem ['(', ')', '-', '>', ',' , '}', '{']) t
  in
    List.foldl listTypes [] modules


-- Transform ["Maybe","foo","Result","bar","Maybe","foo"] --> ["Maybe","a","Result","b","Maybe","a"]

simplifyTypeParams :: Text -> (Map Text Text, [Char], [Text]) -> (Map Text Text, [Char], [Text])
simplifyTypeParams type_ (used, options, acc) =
  if not . Char.isLower $ Data.Text.head type_ then
      (used, options, type_ : acc)
  else
    case Map.lookup type_ used of
        Just simpleName ->
          (used, options, simpleName : acc)
        Nothing ->
          (Map.insert type_ simpleName used, drop 1 options, simpleName : acc)
          where simpleName = singleton $ List.head options -- can list be empty?
