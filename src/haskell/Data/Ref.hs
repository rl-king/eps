{-# LANGUAGE NamedFieldPuns #-}
module Data.Ref (packageRef, moduleRef, valueRef, toPackages, Ref) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Package as Package



data Ref
  = PackageRef Text
  | ModuleRef Text Text
  | ValueRef Text Text Text
  deriving (Eq, Show, Ord)


packageRef :: Package -> Ref
packageRef Package{packageName} =
  PackageRef packageName


moduleRef :: Package -> Module -> Ref
moduleRef Package{packageName} Module{moduleName} =
  ModuleRef packageName moduleName


valueRef :: Package -> Module -> Text -> Ref
valueRef Package{packageName} Module{moduleName} valueName =
  ValueRef packageName moduleName valueName


toPackages :: Map Text Package -> [Ref] -> [Package]
toPackages packages =
   mapMaybe (toPackage packages)


toPackage :: Map Text Package -> Ref -> Maybe Package
toPackage packages ref =
  case ref of
    PackageRef name ->
      Map.lookup name packages
    ModuleRef name _ ->
      Map.lookup name packages
    ValueRef name _ _ ->
      Map.lookup name packages
