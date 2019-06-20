{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name
  ( Tokens
  , Token
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
import qualified Data.Ord
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.Result as Result
import Search.Result



-- DEFINITIONS


newtype Tokens =
  Tokens { tokens :: Map Token [Result.Info] }
  deriving (Show)


newtype Token =
  Token { token :: Text }
  deriving (Eq, Ord, Show)


size :: Tokens -> Int
size (Tokens tokens) =
  Map.size tokens


keys :: Tokens -> [Token]
keys (Tokens tokens) =
  Map.keys tokens


-- QUERY


query :: Text -> Tokens -> [(Result.Info, Int)]
query term (Tokens index) =
  List.take 30 . List.sortOn (Data.Ord.Down . snd) .
  Map.toList $ List.foldl lookupTokens Map.empty tokens
  where
    tokens = Token <$> Text.words term
    lookupTokens acc termPart =
      case Map.lookup termPart index of
        Nothing ->
          acc
        Just xs ->
          List.foldl (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs



-- PACKAGE NAMES


tokenizePackageNames :: [Package] -> Tokens
tokenizePackageNames =
  Tokens . Map.fromListWith (++) . mapMaybe extractPackageName


extractPackageName :: Package -> Maybe (Token, [Result.Info])
extractPackageName package@Package{_pName} =
  case Text.splitOn "/" _pName of
    _:name:_ ->
      Just (Token name, [Result.packageRef package])

    _ ->
      Nothing



-- MODULE NAMES


tokenizeModuleNames :: [Package] -> Tokens
tokenizeModuleNames =
  Tokens . Map.fromListWith (++) . concatMap extractModuleName


extractModuleName :: Package -> [(Token, [Result.Info])]
extractModuleName package@Package{_pModules} =
  let
    toKeyValuePairs module_@Module{_mName} =
      (Token _mName, [Result.moduleRef package module_])
  in
    List.map toKeyValuePairs $ Map.elems _pModules



-- VALUE NAMES


tokenizeValueNames :: [Package] -> Tokens
tokenizeValueNames =
  Tokens . Map.fromListWith (++) . concatMap extractValueName


extractValueName :: Package -> [(Token, [Result.Info])]
extractValueName package@Package{_pModules} =
  let
    toKeyValuePairs acc module_@Module{_mDefs} =
      List.foldl (toInfo module_) [] (Map.elems _mDefs) ++ acc

    toInfo module_ acc def =
      case def of
        TypeAlias n _ _ _ ->  toPair module_ n : acc
        Binop n _ _ ->        toPair module_ n : acc
        Value_ n _ _ ->       toPair module_ n : acc
        CustomType _ _ _ _ -> acc

    toPair module_ typeName =
      (Token typeName, [Result.valueRef package module_ typeName])
  in
    List.foldl toKeyValuePairs [] _pModules
