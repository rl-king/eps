{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.TypeSig
  ( TypeSigIndex
  , toList
  , query
  , tokenize
  , toTokens
  ) where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Data.Index as Index
import qualified Search.Result as Result



-- DEFINITIONS


type TypeSigIndex =
  Index.Index (Text, Int) Result.Info


type TypeSigToken =
  Index.Token (Text, Int)


-- QUERY


query :: Text -> TypeSigIndex -> [(Result.Info, Int)]
query term index =
  Map.toList . List.foldl' lookupTokens Map.empty $ toTokens term
  where
    lookupTokens acc termPart =
      case Index.lookup termPart index of
        Nothing ->
          acc
        Just xs ->
          List.foldl' (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs



-- FUNCTION SIGNATURES


tokenize :: Package -> TypeSigIndex -> TypeSigIndex
tokenize package index =
  foldl' (\ts (k, v) -> Index.insertList k v ts) index (extract package)


extract :: Package -> [(TypeSigToken, [Result.Info])]
extract package@Package{_pModules} =
  let
    toKeyValuePairs acc module_@Module{_mDefs} =
      List.foldl' (toInfo module_) [] (Map.elems _mDefs) ++ acc

    toInfo module_ acc def =
      case def of
        TypeAlias n _ _ t ->  toPair module_ n t ++ acc
        Binop n _ t ->        toPair module_ n t ++ acc
        Value_ n _ t ->       toPair module_ n t ++ acc
        CustomType{} ->       acc

    toPair module_ typeName type_ =
      (\x -> (x ,[Result.valueRef package module_ typeName])) <$> toTokens type_
  in
    List.foldl' toKeyValuePairs [] _pModules


{-|
  toTokens "(a -> Task x b) -> Task x a -> Task x b"
  --> [("->",3),("Task",3),("a",2),("b",3),("c",2)]
-}
toTokens :: Text -> [TypeSigToken]
toTokens =
  let
    removeModules =
      fmap (last . Text.splitOn ".")

    removeChars =
      Text.filter (not . flip elem ['(', ')', ',' , '}', '{'])
  in
    fmap Index.toToken . countOccurrences . removeModules .
    simplifyTypeVariables . Text.words . removeChars


toList :: [TypeSigToken] -> [(Text, Int)]
toList =
  fmap Index.unpackToken


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


countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences =
  Map.toList . List.foldl' (\acc k -> Map.insertWith (+) k 1 acc) Map.empty
