{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Docs
  ( Tokens
  , Token
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
import qualified Search.ResultInfo as ResultInfo
import Search.ResultInfo (ResultInfo)
import qualified NLP.Stemmer as Stem



-- DEFINITIONS


newtype Tokens =
  Tokens { tokens :: Map Token [ResultInfo] }
  deriving (Show)


newtype Token =
  Token { token :: Text }
  deriving (Eq, Ord, Show)



-- QUERY


query :: Text -> Tokens -> [ResultInfo]
query term (Tokens idx) =
  []



-- DOCS


tokenizeSummaries :: [Package] -> Tokens
tokenizeSummaries =
  -- Map.filter ((>) 400 . length) .
  Tokens . Map.fromListWith (++) . concatMap extractSummary


extractSummary :: Package -> [(Token, [ResultInfo])]
extractSummary package@Package{summary} =
  let
    toPair token =
      (token, [ResultInfo.packageRef package])
  in
    List.map toPair (toTokens summary)



-- COMMENTS


tokenizeComments :: [Package] -> Tokens
tokenizeComments =
  -- Map.filter ((>) 400 . length) .
  Tokens . Map.fromListWith (++) . concatMap extractComments


extractComments :: Package -> [(Token, [ResultInfo])]
extractComments package@Package{modules} =
  let
    toComments acc module_@Module{customTypes, values, binops, aliases} =
      List.map (valueComment module_) values ++
      List.map (binopComment module_) binops ++
      List.map (customTypeComment module_) customTypes ++
      List.map (aliasesComment module_) aliases ++ acc

    valueComment module_ (Value_ typeName comment _) =
      toPair module_ typeName comment

    binopComment module_ (Binop typeName comment _) =
      toPair module_ typeName comment

    customTypeComment module_ (CustomType typeName comment _ _) =
      toPair module_ typeName comment

    aliasesComment module_ (TypeAlias typeName comment _ _) =
      toPair module_ typeName comment

    toPair module_ typeName comment =
      List.map (toPair_ module_ typeName) (toTokens comment)

    toPair_ module_ typeName token =
      (token, [ResultInfo.valueRef package module_ typeName])
  in
    concat $ List.foldl toComments [] modules



-- TOKENIZATION


toTokens :: Text -> [Token]
toTokens =
  List.map Token . List.nub . Stem.run . filterStopWords .
  Text.words . Text.toLower . filterPunctuation


filterPunctuation :: Text -> Text
filterPunctuation =
  Text.filter (not . flip Set.member punctuation)


punctuation  :: Set Char
punctuation =
  Set.fromList [ '.' , ',']


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
