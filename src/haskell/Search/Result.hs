{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Search.Result (packageRef, moduleRef, valueRef, toSearchResults, Info, Result) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Package as Package
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import GHC.Generics



type PackageName = Text
type ModuleName = Text
type DefName = Text


data Result =
  Result
  { _rCategory :: Text
  , _rPackageName :: Text
  , _rModuleName :: Text
  , _rValueName :: Text
  , _rValueComment :: Text
  , _rTypeSignature :: Text
  } deriving (Generic, Ord, Eq)


instance Aeson.ToJSON Result


data Info
  = PackageRef PackageName
  | ModuleRef PackageName ModuleName
  | ValueRef PackageName ModuleName DefName
  deriving (Eq, Show, Ord)


packageRef :: Package -> Info
packageRef Package{_pName} =
  PackageRef _pName


moduleRef :: Package -> Module -> Info
moduleRef Package{_pName} Module{_mName} =
  ModuleRef _pName _mName


valueRef :: Package -> Module -> Text -> Info
valueRef Package{_pName} Module{_mName} =
  ValueRef _pName _mName


toSearchResults :: Map Text Package -> [Info] -> [Result]
toSearchResults packages =
  mapMaybe (toSearchResult packages)


toSearchResult :: Map Text Package -> Info -> Maybe Result
toSearchResult packages ref =
  case ref of
    PackageRef name ->
      Nothing
    ModuleRef name _ ->
      Nothing
    ValueRef _pName _mName _dName -> do
      package <- Map.lookup _pName packages
      module' <- Map.lookup _mName (_pModules package)
      def <- Map.lookup _dName (_mDefs module')
      return Result
        { _rCategory = Package.category def
        , _rPackageName = _pName
        , _rModuleName = _mName
        , _rValueName = _dName
        , _rValueComment = Package.comment def
        , _rTypeSignature = Package.typeSig def
        }
