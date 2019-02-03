{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Search where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)
import Control.Monad

import qualified Search.Result as SR
import qualified Token.Docs
import qualified Token.TypeSig
import qualified Token.Name
import Data.Package (Package)



-- INDEX


data Index =
  Index
  { typeSignatures :: Map (Text, Int) [SR.Result]
  , valueNames :: Map (Text, Int) [SR.Result]
  , moduleNames :: Map (Text, Int) [SR.Result]
  , packageNames :: Map (Text, Int) [SR.Result]
  , docs :: Map (Text, Int) [SR.Result]
  , comments :: Map (Text, Int) [SR.Result]
  }


index :: [Package] -> Index
index packages = Index
  (Token.TypeSig.tokenize packages)
  (Token.Name.tokenizeValueNames packages)
  (Token.Name.tokenizeModuleNames packages)
  (Token.Name.tokenizePackageNames packages)
  (Token.Docs.tokenizeDocs packages)
  (Token.Docs.tokenizeComments packages)


info :: Index -> IO ()
info index =
  let
    tl = Map.toList . Map.map length
    results = putStrLn . unlines . map show . take 50 . reverse

    ts = tl $ typeSignatures index
    vn = tl $ valueNames index
    mn = tl $ moduleNames index
    pn = tl $ packageNames index
    d = tl $ docs index
    c = tl $ comments index
  in
    do
      mapM_ results [ts, vn, mn, pn, List.sortOn snd d, List.sortOn snd c]
      print $ show (length ts) ++ " : indexed type signatures"
      print $ show (length vn) ++ " : indexed value names"
      print $ show (length mn) ++ " : indexed module names"
      print $ show (length pn) ++ " : indexed package names"
      print $ show (length d) ++ " : indexed docs"
      print $ show (length c) ++ " : indexed comments"


-- SEARCHING


perform ::  Text -> Index -> [SR.Result]
perform term Index{typeSignatures, valueNames, comments, docs} =
  let
    get selectedIndex acc termPart =
      case Map.lookup termPart selectedIndex of
        Just xs -> List.foldl (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs
        Nothing -> acc

    search terms selectedIndex =
      List.map fst $ List.take 100 $ List.filter ((==) (length terms) . snd) $
      Map.toList $ List.foldl (get selectedIndex) Map.empty terms
  in
    -- if searchTypeSigs term then
    --   search (Token.TypeSig.typeSigToToken term) typeSignatures
    -- else
      search (List.map (\x -> (x, 1)) (Text.words term)) comments


searchTypeSigs :: Text -> Bool
searchTypeSigs term =
  (List.any (Char.isUpper . Text.head) $ Text.words term) ||
  (List.elem "->" $ Text.words term)
