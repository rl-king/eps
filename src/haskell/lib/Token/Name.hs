{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name
  ( Tokens
  , Token
  , empty
  , size
  , keys
  , query
  , tokenizePackageNames
  , tokenizeModuleNames
  , tokenizeValueNames
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.Result as Result



-- DEFINITIONS


newtype Tokens =
  Tokens { tokens :: Map Token [Result.Info] }
  deriving (Show)


newtype Token =
  Token { token :: Text }
  deriving (Eq, Ord, Show)


empty :: Tokens
empty =
  Tokens Map.empty


size :: Tokens -> Int
size (Tokens tokens) =
  Map.size tokens


keys :: Tokens -> [Token]
keys (Tokens tokens) =
  Map.keys tokens


-- QUERY


query :: Text -> Int -> Tokens -> Map Result.Info Int
query term points (Tokens index) =
  List.foldl' lookupTokens Map.empty $
  Token <$> Text.words term
  where
    lookupTokens acc termPart =
      case Map.lookup termPart index of
        Nothing ->
          acc
        Just xs ->
          List.foldl' (\acc_ x -> Map.insertWith (+) x points acc_) acc xs



-- PACKAGE NAMES


tokenizePackageNames :: Package -> Tokens -> Tokens
tokenizePackageNames package (Tokens tokens) =
  Tokens $ foldl' (\ts (k, v) -> Map.insertWith (++) k v ts) tokens (extractPackageName package)


extractPackageName :: Package -> Maybe (Token, [Result.Info])
extractPackageName package@Package{_pName} =
  case Text.splitOn "/" _pName of
    _:name:_ ->
      Just (Token name, [Result.packageRef package])

    _ ->
      Nothing



-- MODULE NAMES


tokenizeModuleNames :: Package -> Tokens -> Tokens
tokenizeModuleNames package (Tokens tokens) =
  Tokens $ foldl' (\ts (k, v) -> Map.insertWith (++) k v ts) tokens (extractModuleName package)


extractModuleName :: Package -> [(Token, [Result.Info])]
extractModuleName package@Package{_pModules} =
  let
    toKeyValuePairs module_@Module{_mName} =
      (\n -> (Token n, [Result.moduleRef package module_])) <$> Text.splitOn "." _mName
  in
    concatMap toKeyValuePairs (Map.elems _pModules)



-- VALUE NAMES


tokenizeValueNames :: Package -> Tokens -> Tokens
tokenizeValueNames package (Tokens tokens) =
  Tokens $ foldl' (\ts (k, v) -> Map.insertWith (++) k v ts) tokens (extractValueName package)


extractValueName :: Package -> [(Token, [Result.Info])]
extractValueName package@Package{_pModules} =
  let
    toKeyValuePairs acc module_@Module{_mDefs} =
      List.foldl' (toInfo module_) [] (Map.elems _mDefs) ++ acc

    toInfo module_ acc def =
      case def of
        TypeAlias n _ _ _ ->  toPair module_ n : acc
        Binop n _ _ ->        toPair module_ n : acc
        Value_ n _ _ ->       toPair module_ n : acc
        CustomType _ _ _ _ -> acc

    toPair module_ typeName =
      (Token (Text.toLower typeName), [Result.valueRef package module_ typeName])
  in
    List.foldl' toKeyValuePairs [] _pModules
