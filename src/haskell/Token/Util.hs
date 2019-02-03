module Token.Util where

import qualified Data.List as List
import qualified Data.Map.Strict as Map



countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences =
  Map.toList . List.foldl (\acc k -> Map.insertWith (+) k 1 acc) Map.empty
