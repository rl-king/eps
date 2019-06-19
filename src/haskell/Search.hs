{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Search where

import Data.Foldable
import qualified Data.Char as Char
import qualified Data.List as List
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
  { typeSignatures :: TypeSig.Tokens
  , valueNames :: Name.Tokens
  , moduleNames :: Name.Tokens
  , packageNames :: Name.Tokens
  , summaries :: Docs.Tokens
  , comments :: Docs.Tokens
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


perform :: Text -> Map Text Package -> Index -> [Result]
perform term packages Index{typeSignatures, valueNames, comments, summaries} =
    -- case strategy term of
    --   TypeSigs ->
        Result.toSearchResults packages $
        TypeSig.query term typeSignatures

      -- _ ->
      --   []



-- STRATEGY


data Strategy
  = All
  | TypeSigs
  | None


strategy :: Text -> Strategy
strategy term =
  if searchTypeSigs term then
    TypeSigs
  else
    None


searchTypeSigs :: Text -> Bool
searchTypeSigs term =
  (List.any (Char.isUpper . Text.head) $ Text.words term)
  || (List.elem "->" $ Text.words term)
