{-# LANGUAGE NamedFieldPuns #-}
module Data.Ref (packageRef, moduleRef, valueRef, Ref) where

import Data.Text (Text)
import Data.Package as P



data Ref
  = PackageRef Text
  | ModuleRef Text Text
  | ValueRef Text Text Text


packageRef :: Package -> Ref
packageRef Package{packageName} =
  PackageRef packageName


moduleRef :: Package -> Module -> Ref
moduleRef Package{packageName} Module{moduleName} =
  ModuleRef packageName moduleName


valueRef :: Package -> Module -> Text -> Ref
valueRef Package{packageName} Module{moduleName} valueName =
  ValueRef packageName moduleName valueName
