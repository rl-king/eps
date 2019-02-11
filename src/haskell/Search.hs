{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Search where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Search.Result
import qualified Token.Docs as Docs
import qualified Token.TypeSig as TypeSig
import qualified Token.Name as Name
import Data.Package (Package)



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


-- info :: Index -> IO ()
-- info index =
--   let
--     -- tl (TypeSig.Tokens m) = Map.toList $ Map.map length m
--     -- results = putStrLn . unlines . map show . take 50 . reverse

--     -- ts = tl $ typeSignatures index
--     -- vn = tl $ valueNames index
--     -- mn = tl $ moduleNames index
--     -- pn = tl $ packageNames index
--     -- d = tl $ summaries index
--     -- c = tl $ comments index
--   in
--     do
--       -- mapM_ results [ts, vn, mn, pn, List.sortOn snd d, List.sortOn snd c]
--       -- print $ show (length ts) ++ " : indexed type signatures"
--       -- print $ show (length vn) ++ " : indexed value names"
--       -- print $ show (length mn) ++ " : indexed module names"
--       -- print $ show (length pn) ++ " : indexed package names"
--       -- print $ show (length d) ++ " : indexed summaries"
--       -- print $ show (length c) ++ " : indexed comments"



-- SEARCHING


perform :: Text -> Index -> [Search.Result.Result]
perform term Index{typeSignatures, valueNames, comments, summaries} =
    case strategy term of
      TypeSigs ->
        []

      _ ->
        []



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
