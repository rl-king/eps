{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name
  ( NameIndex
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
import qualified Data.Index as Index
import qualified Search.Result as Result



-- DEFINITIONS


type NameIndex =
  Index.Index Text Result.Info


type NameToken =
  Index.Token Text


-- QUERY


query :: Text -> Int -> NameIndex -> Map Result.Info Int
query term points index =
  List.foldl' lookupTokens Map.empty $
  Index.toToken <$> Text.words term
  where
    lookupTokens acc termPart =
      case Index.lookup termPart index of
        Nothing ->
          acc
        Just xs ->
          List.foldl' (\acc_ x -> Map.insertWith (+) x points acc_) acc xs



-- PACKAGE NAMES


tokenizePackageNames :: Package -> NameIndex -> NameIndex
tokenizePackageNames package index =
  foldl' (\ts (k, v) -> Index.insertList k v ts) index (extractPackageName package)


extractPackageName :: Package -> Maybe (NameToken, [Result.Info])
extractPackageName package@Package{_pName} =
  case Text.splitOn "/" _pName of
    _:name:_ ->
      Just (Index.toToken name, [Result.packageRef package])

    _ ->
      Nothing



-- MODULE NAMES


tokenizeModuleNames :: Package -> NameIndex -> NameIndex
tokenizeModuleNames package index =
  foldl' (\ts (k, v) -> Index.insertList k v ts) index (extractModuleName package)


extractModuleName :: Package -> [(NameToken, [Result.Info])]
extractModuleName package@Package{_pModules} =
  let
    toKeyValuePairs module_@Module{_mName} =
      (\n -> (Index.toToken n, [Result.moduleRef package module_])) <$> Text.splitOn "." _mName
  in
    concatMap toKeyValuePairs (Map.elems _pModules)



-- VALUE NAMES


tokenizeValueNames :: Package -> NameIndex -> NameIndex
tokenizeValueNames package index =
  foldl' (\ts (k, v) -> Index.insertList k v ts) index (extractValueName package)


extractValueName :: Package -> [(NameToken, [Result.Info])]
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
      (Index.toToken (Text.toLower typeName), [Result.valueRef package module_ typeName])
  in
    List.foldl' toKeyValuePairs [] _pModules
