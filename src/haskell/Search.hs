{-# LANGUAGE OverloadedStrings #-}

module Search where

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

import Data.Package


perform ::  BS.ByteString -> [Package] -> [Package]
perform term packages =
  let
    asMap =
      Map.fromList $ List.map (\p@(Package n _ _ _) -> (n, p)) packages

    rank x check weight =
      if byteStringContains term (TE.encodeUtf8 check) then
        (weight, x)
      else
        (0, x)

    inTitle =
      List.map (\(Package x _ _ _) -> rank x x 1) packages

    inSummary =
      List.map (\(Package x s _ _) -> rank x s 0.5) packages

    merge (r1, a) (r2, _) =
      (r1 + r2, a)

    get acc (_, i) =
      case Map.lookup i asMap of
        Just x -> x : acc
        Nothing -> acc
  in
    List.foldl get [] $
    List.take 10 $
    List.sortOn fst $
    List.filter ((/=) 0 . fst) $
    zipWith merge inTitle inSummary


byteStringContains :: BS.ByteString -> BS.ByteString -> Bool
byteStringContains term bs =
  not . BS.null . snd $ BS.breakSubstring term bs
