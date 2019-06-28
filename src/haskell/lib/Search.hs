{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Search
  ( emptyIndex
  , insert
  , indexStats
  , perform
  , Index
  )
where

import Data.Foldable
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Ord as Ord
import Data.Package (Package)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Search.Result as Result
import Search.Result (Result)
import qualified Token.Docs as Docs
import qualified Token.Name as Name
import qualified Token.TypeSig as TypeSig

-- INDEX
data Index
  = Index
      { _iTypeSignatures :: TypeSig.Tokens
      , _iDefNames :: Name.Tokens
      , _iModuleNames :: Name.Tokens
      , _iPackageNames :: Name.Tokens
      , _iSummaries :: Docs.Tokens
      , _iComments :: Docs.Tokens
      }

insert :: Package -> Index -> Index
insert package (Index ts vn mn pn d c) =
  Index
    (TypeSig.tokenize package ts)
    (Name.tokenizeValueNames package vn)
    (Name.tokenizeModuleNames package mn)
    (Name.tokenizePackageNames package pn)
    (Docs.tokenizeSummaries package d)
    (Docs.tokenizeComments package c)

emptyIndex :: Index
emptyIndex =
  Index
    TypeSig.empty
    Name.empty
    Name.empty
    Name.empty
    Docs.empty
    Docs.empty

indexStats :: Index -> IO ()
indexStats (Index ts vn mn pn d c) =
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
  let packageNames = Name.query term 10 (_iPackageNames index)
      moduleNames = Name.query term 8 (_iModuleNames index)
      defNames = Name.query term 6 (_iDefNames index)
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
  in case toScope term of
       TypeSigs ->
         ( "TYPESIGS" : info
         , Result.toSearchResults packages . take 10 . List.sortOn (Ord.Down . snd) $
           Map.toList packageNames ++
           Map.toList moduleNames ++
           rank packageNames moduleNames (Map.toList defNames) ++
           rank packageNames moduleNames typeSigs
         )
       ModulesAndPackages ->
         ( "MODULES AND PACKAGES" : info
         , Result.toSearchResults packages . take 10 . List.sortOn (Ord.Down . snd) $
           Map.toList packageNames ++
           Map.toList moduleNames ++
           rank packageNames moduleNames (Map.toList defNames)
         )
       Rest ->
         ( "REST" : info
         , Result.toSearchResults packages . take 10 . List.sortOn (Ord.Down . snd) $
           Map.toList packageNames ++
           Map.toList moduleNames ++
           Map.toList comments ++
           Map.toList summaries ++
           rank packageNames moduleNames (Map.toList defNames)
         )

rank
  :: Map Result.Info Int
  -> Map Result.Info Int
  -> [(Result.Info, Int)]
  -> [(Result.Info, Int)]
rank packages modules typeSigs =
  promote <$> typeSigs
  where
    promote (info, points) = case info of
      Result.ValueRef pName mName _ ->
        let pMember = Map.member (Result.PackageRef pName) packages
            mMember = Map.member (Result.ModuleRef pName mName) modules
        in case (pMember, mMember) of
             (True, True) -> (info, points + 15)
             (False, True) -> (info, points + 10)
             (True, False) -> (info, points + 5)
             (False, False) -> (info, points)
      _ ->
        (info, points)

-- SCOPE
data Scope
  = TypeSigs
  | ModulesAndPackages
  | Rest

toScope :: Text -> Scope
toScope query
  | Text.isInfixOf "->" query = TypeSigs
  | Text.toLower query /= query = ModulesAndPackages
  | otherwise = Rest
