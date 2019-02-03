{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.TypeSig where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
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
    toKeyValuePairs acc Module{moduleName, values, binops, aliases} =
      List.concatMap (valueToPair moduleName) values ++
      List.concatMap (aliasesToPair moduleName) aliases ++
      List.concatMap (binopToPair moduleName) binops ++ acc

    aliasesToPair moduleName (TypeAlias typeName comment _ type_) =
      toPair moduleName typeName comment type_

    binopToPair moduleName (Binop typeName comment type_) =
      toPair moduleName typeName comment type_

    valueToPair moduleName (Value_ typeName comment type_) =
      toPair moduleName typeName comment type_

    toPair moduleName typeName comment type_ =
      List.map (\x -> (x ,[SR.Result SR.Value packageName moduleName typeName comment type_])) $
      typeSigToToken type_

  in
    List.foldl toKeyValuePairs [] modules


{-|
  typeSigToToken "(a -> Task x b) -> Task x a -> Task x b"
  --> [("->",3),("Task",3),("a",2),("b",3),("c",2)]
-}
typeSigToToken :: Text -> [(Text, Int)]
typeSigToToken =
  let
    removeModules =
      List.map (last . Text.splitOn ".")

    removeChars =
      Text.filter (not . flip elem ['(', ')', ',' , '}', '{'])
  in
    countOccurrences . removeModules . simplifyTypeVariables . Text.words . removeChars


countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences =
  Map.toList . List.foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty


{-|
  simplifyTypeVariables ["Maybe","foo","Result","bar","Maybe","foo"]
  --> ["Maybe","a","Result","b","Maybe","a"]
-}
simplifyTypeVariables :: [Text] -> [Text]
simplifyTypeVariables =
  (\(_,_,x) -> x) . List.foldr toLowercaseChar (Map.empty, ['a'..], [])


toLowercaseChar :: Text -> (Map Text Text, [Char], [Text]) -> (Map Text Text, [Char], [Text])
toLowercaseChar type_ (used, options, acc) =
  if not (Char.isLower $ Text.head type_) || isReserved type_ then
    (used, options, type_ : acc)
  else
    case Map.lookup type_ used of
      Just simpleName ->
        (used, options, simpleName : acc)
      Nothing ->
        (Map.insert type_ simpleName used, drop 1 options, simpleName : acc)
        where simpleName = Text.singleton $ List.head options


isReserved :: Text -> Bool
isReserved type_ =
  List.any (\x -> Text.take (Text.length x) type_ == x)
  ["number", "comparable", "appendable", "compappend"]
