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
  Map.fromListWith (++) . concatMap extractSummary


extractSummary :: Package -> [((Text, Int), [SR.Result])]
extractSummary Package{packageName, summary} =
  let
    toPair word =
      (word, [SR.Result SR.Value packageName "" "" "" ""])
  in
    List.map toPair . countOccurrences $ filterStopWords (Text.words summary)


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
