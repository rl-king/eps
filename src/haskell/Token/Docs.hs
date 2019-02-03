{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Token.Docs where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
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
    List.map toPair . countOccurrences $ filterStopWords (Text.words summary)



-- COMMENTS


tokenizeComments :: [Package] -> Tokens
tokenizeComments =
  Map.filter ((>) 400 . length) .
  Map.fromListWith (++) . concatMap extractComments


extractComments :: Package -> [((Text, Int), [SR.Result])]
extractComments Package{packageName, modules} =
  let
    toComments acc Module{comment, customTypes, values, binops, aliases} =
      toPair comment :
      List.map valueComment values ++
      List.map binopComment binops ++
      List.map customTypeComment customTypes ++
      List.map aliasesComment aliases ++ acc

    valueComment (Value_ _ comment _) =
      toPair comment

    binopComment (Binop _ comment _) =
      toPair comment

    customTypeComment (CustomType _ comment _ _) =
      toPair comment

    aliasesComment (TypeAlias _ comment _ _) =
      toPair comment

    toPair =
      List.map toPair_ . List.nub . filterStopWords . Text.words . cleanUp

    toPair_ x =
      ((x, 1), [SR.Result SR.Value packageName "" "" "" ""])
  in
    concat $ List.foldl toComments [] modules


-- HELPERS

cleanUp :: Text -> Text
cleanUp =
  Text.filter (not . flip Set.member cleanUpChars)


cleanUpChars :: Set Char
cleanUpChars =
  Set.fromList
  [ '.'
  , ','
  ]

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
