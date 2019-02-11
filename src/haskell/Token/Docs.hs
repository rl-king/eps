{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Docs where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Token.Stemmer as Stem
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

import Data.Package as Package
import Data.Ref as Ref
import Token.Util



type Tokens =
  Map (Text, Int) [Ref.Ref]


-- DOCS


tokenizeSummaries :: [Package] -> Tokens
tokenizeSummaries =
  -- Map.filter ((>) 400 . length) .
  Map.fromListWith (++) . concatMap extractSummary


extractSummary :: Package -> [((Text, Int), [Ref.Ref])]
extractSummary package@Package{summary} =
  let
    toPair word =
      (word, [Ref.packageRef package])
  in
    List.map toPair . countOccurrences $ Stem.run $ filterStopWords (Text.words summary)



-- COMMENTS


tokenizeComments :: [Package] -> Tokens
tokenizeComments =
  -- Map.filter ((>) 400 . length) .
  Map.fromListWith (++) . concatMap extractComments


extractComments :: Package -> [((Text, Int), [Ref.Ref])]
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
      ((token, 1), [Ref.valueRef package module_ typeName])
  in
    concat $ List.foldl toComments [] modules



-- HELPERS


toTokens :: Text -> [Text]
toTokens =
  List.nub . Stem.run . filterStopWords . Text.words . Text.toLower .
  Text.filter (not . flip Set.member cleanUpChars)


cleanUpChars :: Set Char
cleanUpChars =
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
