{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Docs
  ( Tokens
  , Token
  , empty
  , size
  , query
  , tokenizeSummaries
  , tokenizeComments
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

import Data.Package as Package
import qualified Search.Result as Result
import Search.Result
import qualified NLP.Stemmer as Stem



-- DEFINITIONS


newtype Tokens =
  Tokens { tokens :: Map Token [Result.Info] }
  deriving (Show)


newtype Token =
  Token { token :: Text }
  deriving (Eq, Ord, Show)


empty :: Tokens
empty =
  Tokens Map.empty


size :: Tokens -> Int
size (Tokens tokens) =
  Map.size tokens


-- QUERY


query :: Text -> Tokens -> [(Result.Info, Int)]
query term (Tokens index) =
  Map.toList . List.foldl lookupTokens Map.empty $ toTokens term
  where
    lookupTokens acc termPart =
      case Map.lookup termPart index of
        Nothing ->
          acc
        Just xs ->
          List.foldl (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs


-- DOCS


tokenizeSummaries :: Package -> Tokens -> Tokens
tokenizeSummaries package (Tokens tokens) =
  Tokens $ foldl (\ts (k, v) -> Map.insertWith (++) k v ts) tokens (extractSummary package)


extractSummary :: Package -> [(Token, [Result.Info])]
extractSummary package@Package{_pSummary} =
  let
    toPair token =
      (token, [Result.packageRef package])
  in
    toPair <$> toTokens _pSummary



-- COMMENTS


tokenizeComments :: Package -> Tokens -> Tokens
tokenizeComments package (Tokens tokens) =
  Tokens $ foldl (\ts (k, v) -> Map.insertWith (++) k v ts) tokens (extractComments package)


extractComments :: Package -> [(Token, [Result.Info])]
extractComments package@Package{_pModules} =
  let
    toComments acc module_@Module{_mDefs} =
      List.foldl (toInfo module_) [] (Map.elems _mDefs) ++ acc

    toInfo module_ acc def =
      case def of
        TypeAlias n c _ _ ->  toPair module_ n c : acc
        Binop n c _ ->        toPair module_ n c : acc
        Value_ n c _ ->       toPair module_ n c : acc
        CustomType n c _ _ -> toPair module_ n c : acc

    toPair module_ typeName comment_ =
      toPair_ module_ typeName <$> toTokens comment_

    toPair_ module_ typeName token =
      (token, [Result.valueRef package module_ typeName])
  in
    concat $ List.foldl toComments [] _pModules



-- TOKENIZATION


toTokens :: Text -> [Token]
toTokens =
  fmap Token . List.nub . Stem.run . filterStopWords .
  Text.words . Text.toLower . filterPunctuation


filterPunctuation :: Text -> Text
filterPunctuation =
  Text.filter (not . flip Set.member punctuation)


punctuation  :: Set Char
punctuation =
  Set.fromList ['.' , ',']


filterStopWords :: [Text] -> [Text]
filterStopWords =
    List.filter (not . flip Set.member stopWords)


stopWords :: Set Text
stopWords =
  Set.fromList
  [ "a"
  , "A"
  , "An"
  , "about"
  , "all"
  , "also"
  , "an"
  , "and"
  , "are"
  , "as"
  , "at"
  , "based"
  , "be"
  , "been"
  , "but"
  , "by"
  , "can"
  , "do"
  , "elm"
  , "Elm"
  , "Elm."
  , "for"
  , "from"
  , "functions"
  , "get"
  , "has"
  , "have"
  , "how"
  , "i"
  , "if"
  , "implementation"
  , "in"
  , "interface"
  , "into"
  , "is"
  , "it"
  , "library"
  , "like"
  , "much"
  , "no"
  , "now"
  , "of"
  , "off"
  , "on"
  , "one"
  , "only"
  , "or"
  , "other"
  , "out"
  , "over"
  , "package"
  , "see"
  , "should"
  , "simple"
  , "so"
  , "some"
  , "support"
  , "than"
  , "that"
  , "the"
  , "The"
  , "their"
  , "them"
  , "then"
  , "this"
  , "to"
  , "up"
  , "using"
  , "when"
  , "where"
  , "which"
  , "with"
  , "within"
  , "you"
  , "your"
  ]
