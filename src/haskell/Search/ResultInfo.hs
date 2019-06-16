{-# LANGUAGE NamedFieldPuns #-}
module Search.ResultInfo (packageRef, moduleRef, valueRef, toPackages, ResultInfo) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Package as Package



data ResultInfo
  = PackageRef Text
  | ModuleRef Text Text
  | ValueRef Text Text Text
  deriving (Eq, Show, Ord)


packageRef :: Package -> ResultInfo
packageRef Package{packageName} =
  PackageRef packageName


moduleRef :: Package -> Module -> ResultInfo
moduleRef Package{packageName} Module{moduleName} =
  ModuleRef packageName moduleName


valueRef :: Package -> Module -> Text -> ResultInfo
valueRef Package{packageName} Module{moduleName} valueName =
  ValueRef packageName moduleName valueName


toPackages :: Map Text Package -> [ResultInfo] -> [Package]
toPackages packages =
   mapMaybe (toPackage packages)


toPackage :: Map Text Package -> ResultInfo -> Maybe Package
toPackage packages ref =
  case ref of
    PackageRef name ->
      Map.lookup name packages
    ModuleRef name _ ->
      Map.lookup name packages
    ValueRef name _ _ ->
      Map.lookup name packages
