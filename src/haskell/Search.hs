{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Search where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Search.Result as SR
import qualified Token.TypeSig
import qualified Token.ValueName
import Data.Package (Package)



-- INDEX


data Index =
  Index
  { typeSignatures :: Map (Text, Int) [SR.Result]
  , valueNames :: Map (Text, Int) [SR.Result]
  }


index :: [Package] -> Index
index packages = Index
  (Token.TypeSig.tokenize packages)
  (Token.ValueName.tokenize packages)


info :: Index -> IO ()
info Index{typeSignatures, valueNames} =
  let
    ts = Map.toList $ Map.map length typeSignatures
    vn = Map.toList $ Map.map length valueNames
  in do
    putStrLn $ unlines . map show $ ts
    putStrLn $ unlines . map show $ vn
    print $ show (length ts) ++ " : indexed type signatures"
    print $ show (length vn) ++ " : indexed value names"


-- SEARCHING


perform ::  Text -> Index -> [SR.Result]
perform term Index{typeSignatures, valueNames} =
  let
    get selectedIndex acc termPart =
      case Map.lookup termPart selectedIndex of
        Just xs -> List.foldl (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs
        Nothing -> acc

    search terms selectedIndex =
      List.map fst $ List.take 100 $ List.filter ((==) (length terms) . snd) $
      Map.toList $ List.foldl (get selectedIndex) Map.empty terms
  in
    if mightBeModuleOrFunction term then
      search (Token.TypeSig.typeSigToToken term) typeSignatures
    else
      search [(term, 1)] valueNames


mightBeModuleOrFunction :: Text -> Bool
mightBeModuleOrFunction term =
  (List.any (Char.isUpper . Text.head) $ Text.words term) ||
  (List.any ("->" ==) $ Text.words term)
