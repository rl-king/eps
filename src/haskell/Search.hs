{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Search where

import Data.Foldable
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)

import Search.Result as Search
import qualified Token.Docs as Docs
import qualified Token.TypeSig as TypeSig
import qualified Token.Name as Name
import Data.Package (Package)
import qualified Search.Result as Result
import Search.Result



-- INDEX


data Index =
  Index
  { _iTypeSignatures :: TypeSig.Tokens
  , _iDefNames :: Name.Tokens
  , _iModuleNames :: Name.Tokens
  , _iPackageNames :: Name.Tokens
  , _iSummaries :: Docs.Tokens
  , _iComments :: Docs.Tokens
  }


index :: [Package] -> Index
index packages = Index
  (TypeSig.tokenize packages)
  (Name.tokenizeValueNames packages)
  (Name.tokenizeModuleNames packages)
  (Name.tokenizePackageNames packages)
  (Docs.tokenizeSummaries packages)
  (Docs.tokenizeComments packages)


info :: Index -> IO ()
info (Index ts vn mn pn d c) =
  traverse_ print
    [ show (TypeSig.size ts) ++ " : indexed type signatures"
    , show (Name.size vn) ++ " : indexed value names"
    , show (Name.size mn) ++ " : indexed module names"
    , show (Name.size pn) ++ " : indexed package names"
    , show (Docs.size d) ++ " : indexed summaries"
    , show (Docs.size c) ++ " : indexed comments"
    ]


-- SEARCHING


perform :: Text -> Map Text Package -> Index -> ([String], [Result])
perform term packages index =
  let
    packageNames = Name.query term (_iPackageNames index)
    moduleNames = Name.query term (_iModuleNames index)
    defNames = Name.query term (_iDefNames index)
    summaries = Docs.query term (_iSummaries index)
    comments = Docs.query term (_iComments index)
    typeSigs = TypeSig.query term (_iTypeSignatures index)
    info =
      [ show (Map.size packageNames) ++ " : package names"
      , show (Map.size moduleNames) ++ " : module names"
      , show (Map.size defNames) ++ " : def names"
      , show (length summaries) ++ " : summaries"
      , show (length comments) ++ " : comments"
      , show (length typeSigs) ++ " : type sigs"
      ]
  in
    ( info
    , Result.toSearchResults packages
      . take 20
      . reverse
      . List.sortOn snd
      $ Map.toList packageNames
      ++ rank packageNames moduleNames typeSigs
    )



rank ::
  Map Result.Info Int ->
  Map Result.Info Int ->
  [(Result.Info, Int)] ->
  [(Result.Info, Int)]
rank packages modules typeSigs =
  promote <$> typeSigs
  where
    promote (info, points) =
      case info of
        Result.ValueRef pName mName _ ->
          let
            pMember = Map.member (Result.PackageRef pName) packages
            mMember = Map.member (Result.ModuleRef pName mName) modules
          in
            case (pMember, mMember) of
              (True, True) -> (info, points + 10)
              (False, True) -> (info, points + 7)
              (True, False) -> (info, points + 5)
              (False, False) -> (info, points)
        _ ->
          (info, points)
