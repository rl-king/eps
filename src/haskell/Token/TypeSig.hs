{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Token.TypeSig where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import Data.Text (Text, head, words, filter, splitOn, singleton)
import Data.Map.Strict (Map)

import Data.Package as Package


type Tokens =
  Map (Text, Int) [Info]

data Info =
  Info
  { packageName_ :: Text
  , moduleName_ :: Text
  , typeName :: Text
  , typeSignature :: Text
  } deriving (Show, Eq, Ord)


tokenize :: [Package] -> Tokens
tokenize packages =
  Map.fromListWith (++) $ concat $ fmap extract packages


extract :: Package -> [((Text, Int), [Info])]
extract (Package{packageName, modules}) =
  let
    listTypes acc Module{moduleName, values} =
      (++) acc $ concat $ List.map (fromValues moduleName) values

    fromValues moduleName (Value_ typeName _ type_) =
      List.map (\x -> (x , [Info packageName moduleName typeName type_])) $
      typeSigToToken type_
  in
    List.foldl listTypes [] modules


typeSigToToken :: Text -> [(Text, Int)]
typeSigToToken =
  let
    removeModules =
      List.map (last . Data.Text.splitOn ".")

    removeChars =
      Data.Text.filter (not . flip elem ['(', ')', ',' , '}', '{', ':'])
  in
    countOccurrences . removeModules . simplifyTypeVariables . Data.Text.words . removeChars


countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences =
  Map.toList . List.foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty


{-

  Replace type variables with a more generalized version
  ["Maybe","foo","Result","bar","Maybe","foo"] --> ["Maybe","a","Result","b","Maybe","a"]

-}
simplifyTypeVariables :: [Text] -> [Text]
simplifyTypeVariables =
      (\(_,_,x) -> x) . List.foldr toLowercaseChar (Map.empty, ['a'..], [])


toLowercaseChar:: Text -> (Map Text Text, [Char], [Text]) -> (Map Text Text, [Char], [Text])
toLowercaseChar type_ (used, options, acc) =
  if not . Char.isLower $ Data.Text.head type_ then
      (used, options, type_ : acc)
  else
    case Map.lookup type_ used of
        Just simpleName ->
          (used, options, simpleName : acc)
        Nothing ->
          (Map.insert type_ simpleName used, drop 1 options, simpleName : acc)
          where simpleName = singleton $ List.head options
