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
import Token.Util
import qualified Search.Result as SR



type Tokens =
  Map (Text, Int) [SR.Result]


-- DOCS


tokenizeDocs :: [Package] -> Tokens
tokenizeDocs =
  Map.filter ((>) 400 . length) .
  Map.fromListWith (++) . concatMap extractSummary


extractSummary :: Package -> [((Text, Int), [SR.Result])]
extractSummary Package{packageName, summary} =
  let
    toPair word =
      (word, [SR.Result SR.Value packageName "" "" "" ""])
  in
    List.map toPair . countOccurrences $ Stem.run $ filterStopWords (Text.words summary)



-- COMMENTS


tokenizeComments :: [Package] -> Tokens
tokenizeComments =
  Map.filter ((>) 400 . length) .
  Map.fromListWith (++) . concatMap extractComments


extractComments :: Package -> [((Text, Int), [SR.Result])]
extractComments Package{packageName, modules} =
  let
    toComments acc Module{moduleName, comment, customTypes, values, binops, aliases} =
      toPair comment moduleName "" :
      List.map (valueComment moduleName) values ++
      List.map (binopComment moduleName) binops ++
      List.map (customTypeComment moduleName) customTypes ++
      List.map (aliasesComment moduleName) aliases ++ acc

    valueComment mn (Value_ typeName comment _) =
      toPair mn typeName comment

    binopComment mn (Binop typeName comment _) =
      toPair mn typeName comment

    customTypeComment mn (CustomType typeName comment _ _) =
      toPair mn typeName comment

    aliasesComment mn (TypeAlias typeName comment _ _) =
      toPair mn typeName comment

    toPair moduleName typeName comment =
      List.map (toPair_ moduleName typeName comment) (toTokens comment)

    toPair_ moduleName typeName c x =
      ((x, 1), [SR.Result SR.Value packageName moduleName typeName c ""])
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
