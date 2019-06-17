{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Search.ResultInfo (packageRef, moduleRef, valueRef, toSearchResults, ResultInfo) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Package as Package
import Search.Result (Result(..))
import qualified Search.Result



data ResultInfo
  = PackageRef Text
  | ModuleRef Text Text
  | ValueRef Text Text Text
  deriving (Eq, Show, Ord)


packageRef :: Package -> ResultInfo
packageRef Package{_pName} =
  PackageRef _pName


moduleRef :: Package -> Module -> ResultInfo
moduleRef Package{_pName} Module{_mName} =
  ModuleRef _pName _mName


valueRef :: Package -> Module -> Text -> ResultInfo
valueRef Package{_pName} Module{_mName} valueName =
  ValueRef _pName _mName valueName


toSearchResults :: Map Text Package -> [ResultInfo] -> [Result]
toSearchResults packages =
  mapMaybe (toSearchResult packages)


toSearchResult :: Map Text Package -> ResultInfo -> Maybe Result
toSearchResult packages ref =
  case ref of
    PackageRef name ->
      Nothing
    ModuleRef name _ ->
      Nothing
    ValueRef _pName moduleName' valueName -> do
      package <- Map.lookup _pName packages
      let module' = filter ((==) moduleName' . _mName) (_pModules package)
      return Result
        { _rType_ = Search.Result.Value
        , _rPackageName = _pName
        , _rModuleName = moduleName'
        , _rValueName = valueName
        , _rValueComment = ""
        , _rTypeSignature = ""
        }
