{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.TypeSig (Tokens, Token, query, tokenize) where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Ord
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
  Token { token :: (Text, Int)}
  deriving (Eq, Ord, Show)



-- QUERY


query :: Text -> Tokens -> [Ref.Ref]
query term (Tokens idx) =
  List.map fst . List.take 30 . List.sortOn (Data.Ord.Down . snd) .
  Map.toList $ List.foldl getTs Map.empty tokens
  where
    tokens = toTokens term
    getTs acc termPart =
      case Map.lookup termPart idx of
        Nothing ->
          acc
        Just xs ->
          List.foldl (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs



-- FUNCTION SIGNATURES


tokenize :: [Package] -> Tokens
tokenize =
  Tokens . Map.fromListWith (++) . concatMap extract


extract :: Package -> [(Token, [Ref.Ref])]
extract package@Package{modules} =
  let
    toKeyValuePairs acc module_@Module{values, binops, aliases} =
      List.concatMap (valueToPair module_) values ++
      List.concatMap (aliasesToPair module_) aliases ++
      List.concatMap (binopToPair module_) binops ++ acc

    aliasesToPair module_ (TypeAlias typeName _ _ type_) =
      toPair module_ typeName type_

    binopToPair module_ (Binop typeName _ type_) =
      toPair module_ typeName type_

    valueToPair module_ (Value_ typeName _ type_) =
      toPair module_ typeName type_

    toPair module_ typeName type_ =
      List.map (\x -> (x ,[Ref.valueRef package module_ typeName])) $
      toTokens type_
  in
    List.foldl toKeyValuePairs [] modules


{-|
  toTokens "(a -> Task x b) -> Task x a -> Task x b"
  --> [("->",3),("Task",3),("a",2),("b",3),("c",2)]
-}
toTokens :: Text -> [Token]
toTokens =
  let
    removeModules =
      List.map (last . Text.splitOn ".")

    removeChars =
      Text.filter (not . flip elem ['(', ')', ',' , '}', '{'])
  in
    List.map Token . countOccurrences . removeModules .
    simplifyTypeVariables . Text.words . removeChars



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
  Map.toList . List.foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty
