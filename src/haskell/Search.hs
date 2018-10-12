{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Search where

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Map.Strict (Map)

import Data.Package as Package

import Token.Value

perform ::  Text -> [Package] -> ValueTokens -> [Text]
perform term packages valueTokens =
  let
    -- asMap =
      -- Map.fromList $ List.map (\p@(Package n _ _ _) -> (n, p)) packages
    --
    -- rank x check weight =
    --   if byteStringContains term (TE.encodeUtf8 check) then
    --     (weight, x)
    --   else
    --     (0, x)

    -- inTitle =
    --   List.map (\(Package x _ _ _) -> rank x x 1) packages

    -- inSummary =
    --   List.map (\(Package x s _ _) -> rank x s 0.5) packages

  --   merge (r1, a) (r2, _) =
  --     (r1 + r2, a)

  --   get (_, i) acc =
  --     case Map.lookup i asMap of
  --       Just x -> x : acc
  --       Nothing -> acc
  in
    case Map.lookup term valueTokens of
      Just xs -> List.map (\(ValueInfo x y z _) -> x <> " " <> y <> " " <> z) xs
      Nothing -> Map.keys valueTokens
    -- zipWith merge inTitle inSummary


byteStringContains :: BS.ByteString -> BS.ByteString -> Bool
byteStringContains term bs =
  not . BS.null . snd $ BS.breakSubstring term bs
