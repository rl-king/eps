{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Name
  ( Tokens
  , Token
  , query
  , tokenizePackageNames
  , tokenizeModuleNames
  , tokenizeValueNames
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.ResultInfo as ResultInfo
import Search.ResultInfo (ResultInfo)



-- DEFINITIONS


newtype Tokens =
  Tokens { tokens :: Map Token [ResultInfo] }
  deriving (Show)


newtype Token =
  Token { token :: Text }
  deriving (Eq, Ord, Show)



-- QUERY


query :: Text -> Tokens -> [ResultInfo]
query term (Tokens idx) =
  []



-- PACKAGE NAMES


tokenizePackageNames :: [Package] -> Tokens
tokenizePackageNames =
  Tokens . Map.fromListWith (++) . map extractPackageName


extractPackageName :: Package -> (Token, [ResultInfo])
extractPackageName package@Package{_pName} =
  case Text.splitOn "/" _pName of
    _:name:_ ->
      (Token name, [ResultInfo.packageRef package])

    _ ->
      (Token _pName, [ResultInfo.packageRef package]) -- Kinda weird, maybe filtermap instead



-- MODULE NAMES


tokenizeModuleNames :: [Package] -> Tokens
tokenizeModuleNames =
  Tokens . Map.fromListWith (++) . concatMap extractModuleName


extractModuleName :: Package -> [(Token, [ResultInfo])]
extractModuleName package@Package{_pModules} =
  let
    toKeyValuePairs module_@Module{_mName} =
      (Token _mName, [ResultInfo.moduleRef package module_])
  in
    List.map toKeyValuePairs _pModules



-- VALUE NAMES


tokenizeValueNames :: [Package] -> Tokens
tokenizeValueNames =
  Tokens . Map.fromListWith (++) . concatMap extractValueName


extractValueName :: Package -> [(Token, [ResultInfo])]
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
      (Token typeName, [ResultInfo.valueRef package module_ typeName])
  in
    List.foldl toKeyValuePairs [] _pModules
