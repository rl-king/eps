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
import qualified Token.Name
import Data.Package (Package)



-- INDEX


data Index =
  Index
  { typeSignatures :: Map (Text, Int) [SR.Result]
  , valueNames :: Map (Text, Int) [SR.Result]
  , moduleNames :: Map (Text, Int) [SR.Result]
  , packageNames :: Map (Text, Int) [SR.Result]
  }


index :: [Package] -> Index
index packages = Index
  (Token.TypeSig.tokenize packages)
  (Token.Name.tokenizeValueNames packages)
  (Token.Name.tokenizeModuleNames packages)
  (Token.Name.tokenizePackageNames packages)


info :: Index -> IO ()
info Index{typeSignatures, valueNames, moduleNames, packageNames} =
  let
    tl = Map.toList . Map.map length
    ts = tl typeSignatures
    vn = tl valueNames
    mn = tl moduleNames
    pn = tl packageNames
  in
    do
      putStrLn $ unlines . map show $ ts
      putStrLn $ unlines . map show $ vn
      putStrLn $ unlines . map show $ mn
      putStrLn $ unlines . map show $ pn
      print $ show (length ts) ++ " : indexed type signatures"
      print $ show (length vn) ++ " : indexed value names"
      print $ show (length mn) ++ " : indexed module names"
      print $ show (length pn) ++ " : indexed package names"


-- SEARCHING


perform ::  Text -> Index -> [SR.Result]
perform term Index{typeSignatures, valueNames, moduleNames, packageNames} =
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
      search [(term, 1)] packageNames


searchTypeSigs :: Text -> Bool
searchTypeSigs term =
  (List.any (Char.isUpper . Text.head) $ Text.words term) ||
  (List.elem "->" $ Text.words term)
