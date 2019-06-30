{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Docs
  ( DocsIndex
  , query
  , tokenizeSummaries
  , tokenizeComments
  ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Map.Strict (Map)
import Data.List (foldl')
import Data.Set (Set)
import Data.Text (Text)

import Data.Package as Package
import qualified Data.Index as Index
import qualified Search.Result as Result
import qualified NLP.Stemmer as Stem



-- DEFINITIONS


type DocsIndex =
  Index.Index Text Result.Info


type DocsToken =
  Index.Token Text



-- QUERY


query :: Text -> DocsIndex -> Map Result.Info Int
query term index =
  List.foldl' lookupTokens Map.empty $ toTokens term
  where
    lookupTokens acc termPart =
      case Index.lookup termPart index of
        Nothing ->
          acc
        Just xs ->
          List.foldl' (\acc_ x -> Map.insertWith (+) x 1 acc_) acc xs


-- DOCS


tokenizeSummaries :: Package -> DocsIndex -> DocsIndex
tokenizeSummaries package index =
  foldl' (\ts (k, v) -> Index.insertList k v ts) index (extractSummary package)


extractSummary :: Package -> [(DocsToken, [Result.Info])]
extractSummary package@Package{_pSummary} =
  let
    toPair token =
      (token, [Result.packageRef package])
  in
    toPair <$> toTokens _pSummary



-- COMMENTS


tokenizeComments :: Package -> DocsIndex -> DocsIndex
tokenizeComments package index =
  foldl' (\ts (k, v) -> Index.insertList k v ts) index (extractComments package)


extractComments :: Package -> [(DocsToken, [Result.Info])]
extractComments package@Package{_pModules} =
  let
    toComments acc module_@Module{_mDefs} =
      List.foldl' (toInfo module_) [] (Map.elems _mDefs) ++ acc

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
    concat $ List.foldl' toComments [] _pModules



-- TOKENIZATION


toTokens :: Text -> [DocsToken]
toTokens =
  fmap Index.toToken . List.nub . Stem.run . filterStopWords .
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
