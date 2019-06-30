module Data.Index
  ( Index
  , Token
  , empty
  , size
  , keys
  , unpackToken
  , toToken
  , insertList
  , lookup
  )
where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (lookup)


newtype Index k v
  = Index {unIndex :: Map (Token k) [v]}
  deriving Show


newtype Token k
  = Token {token :: k}
  deriving (Eq, Ord, Show)


empty :: Index k v
empty =
  Index Map.empty


size :: Index k v -> Int
size =
  Map.size . unIndex


keys :: Index k v -> [Token k]
keys =
  Map.keys . unIndex


toToken :: Ord k => k -> Token k
toToken =
  Token


unpackToken :: Token k -> k
unpackToken =
  token


insertList :: (Ord k) => Token k -> [v] -> Index k v -> Index k v
insertList k v (Index i) =
  Index $ Map.insertWith (++) k v i


lookup :: Ord k => Token k -> Index k v -> Maybe [v]
lookup k (Index i) =
  Map.lookup k i
