{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main ( main) where

import Control.Applicative
import Control.Exception
import Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BS
import qualified Data.List as List
import qualified Data.SearchEngine as Search
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import qualified Snap.Core as Snap
import qualified Snap.Util.FileServe as FileServe
import qualified Snap.Http.Server as Server
import Data.Ix (Ix)
import Data.Text (Text)
import Data.Set (Set)

import Control.Monad
import Control.Exception
import System.IO
-- import System.Directory
import System.Exit
-- PACKAGE


data Package = Package
  { packageName :: Text
  , versions :: [Text]
  } deriving (Show)


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $ \v ->
      Package <$> v .: "name" <*> v .: "versions"


instance Aeson.ToJSON Package where
    toJSON (Package x y) = object ["name" .= x, "versions" .= y]
    toEncoding (Package x y) = pairs ("name" .= x <> "versions" .= y)


-- DOCS


data Docs = Docs
  { moduleName :: Text
  , comment :: Text
  } deriving (Show)


instance Aeson.FromJSON Docs where
  parseJSON =
    Aeson.withObject "Docs" $ \v ->
      Docs <$> v .: "name" <*> v .: "comment"


instance Aeson.ToJSON Docs where
    toJSON (Docs x y) = object ["name" .= x, "comment" .= y]
    toEncoding (Docs x y) = pairs ("name" .= x <> "comment" .= y)


-- MAIN

main :: IO ()
main = do
  packageList <- catch readCache refreshCache
  -- docs <- sequence $ fmap getPackageDocs (take 4 packageList)
  -- BS.writeFile ("./cache/docs.json") (encode docs)
  -- print docs
  let searchEngine = Search.insertDocs packageList initialPkgSearchEngine
  putStrLn "constructing index..."
  evaluate searchEngine >> return ()
  putStrLn $ "search engine invariant: " ++ show (Search.invariant searchEngine)
  -- server packageList


  let loop = do
        putStr "search term> "
        hFlush stdout
        t <- Text.getLine
        unless (Text.null t) $ do
          putStrLn "Ranked results:"
          let rankedResults = Search.queryExplain searchEngine (Text.words t)

          putStr $ unlines
            [ show (Search.overallScore explanation) ++ ": " ++ show pkgname
            | (explanation, pkgname) <- take 10 rankedResults ]

          loop
  return ()
  loop


readCache :: IO [Package]
readCache = do
  file <- BS.readFile "./cache/search.json"
  case Aeson.decode file :: Maybe [Package] of
    Just xs -> return xs
    Nothing -> return []


refreshCache :: IOException -> IO [Package]
refreshCache _ = do
  packages <- getPackageList
  case packages of
    Just xs -> return xs
    Nothing -> return []


getPackageList :: IO (Maybe [Package])
getPackageList = do
  response <- request "https://package.elm-lang.org/search.json"
  let packages = Aeson.decode (Http.responseBody response) :: Maybe [Package]
  BS.writeFile "./cache/search.json" (encode packages)
  return packages


getPackageDocs :: Package -> IO [Docs]
getPackageDocs package = do
  response <- request (toDocsUrl package)
  case Aeson.decode (Http.responseBody response) :: Maybe [Docs] of
    Just xs -> return xs
    Nothing -> return []


toDocsUrl :: Package -> String
toDocsUrl (Package pName pVersions) =
  "https://package.elm-lang.org/packages/" ++ Text.unpack pName ++ "/" ++ (Text.unpack . head) pVersions ++ "/docs.json"



-- REQUEST


request :: String -> IO (Http.Response BS.ByteString)
request path = do
  m <- Http.newManager TLS.tlsManagerSettings
  r <- Http.parseRequest path
  Http.httpLbs r m



-- SERVER


server :: [Package] -> IO ()
server msg =
  Server.quickHttpServe $ site msg


site :: [Package] -> Snap.Snap ()
site packages =
  Snap.ifTop (Snap.writeLBS $ (encode packages)) <|>
  Snap.route [ ("search", searchHandler packages)]


searchHandler :: [Package] -> Snap.Snap ()
searchHandler packages= do
  term <- Snap.getQueryParam "term"
  maybe
    (Snap.writeBS "must specify echo/param in URL")
    (filterPackages packages . TE.decodeUtf8) term


filterPackages :: [Package] -> Text -> Snap.Snap ()
filterPackages packages term =
  Snap.writeLBS $ encode $
  filter (\(Package n v) -> flip (>) 1 . length $ Text.splitOn term n) packages



-- SEARCH


type PkgSearchEngine =
  Search.SearchEngine Package Text PkgDocField Search.NoFeatures

data PkgDocField = NameField
                 -- | SynopsisField
                 -- | DescriptionField
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)


initialPkgSearchEngine :: PkgSearchEngine
initialPkgSearchEngine =
    Search.initSearchEngine pkgSearchConfig defaultSearchRankParameters


pkgSearchConfig :: Search.SearchConfig Package Text PkgDocField Search.NoFeatures
pkgSearchConfig =
    Search.SearchConfig {
      Search.documentKey           = (\(Package n _) -> n),
      Search.extractDocumentTerms  = (\(Package n _) _-> Text.splitOn "/" n),
      Search.transformQueryTerm    = normaliseQueryToken,
      Search.documentFeatureValue  = const Search.noFeatures
  }
  where
    -- extractTokens :: Package -> PkgDocField -> [Text]
    -- extractTokens pkg NameField = extractPackageNameTerms (display $ packageName pkg)
    -- extractTokens pkg SynopsisField = extractSynopsisTerms stopWords (synopsis pkg)
    -- extractTokens pkg DescriptionField = extractDescriptionTerms stopWords (description pkg)

    normaliseQueryToken :: Text -> PkgDocField -> Text
    normaliseQueryToken tok =
      let tokFold = Text.toCaseFold tok
          -- tokStem = stem English tokFold
       in \field -> case field of
                      NameField        -> tokFold
                      -- SynopsisField    -> tokStem
                      -- DescriptionField -> tokStem

defaultSearchRankParameters :: Search.SearchRankParameters PkgDocField Search.NoFeatures
defaultSearchRankParameters =
    Search.SearchRankParameters {
      Search.paramK1,
      Search.paramB,
      Search.paramFieldWeights,
      Search.paramFeatureWeights = Search.noFeatures,
      Search.paramFeatureFunctions = Search.noFeatures,
      Search.paramResultsetSoftLimit = 200,
      Search.paramResultsetHardLimit = 400,
      Search.paramAutosuggestPrefilterLimit  = 500,
      Search.paramAutosuggestPostfilterLimit = 500
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: PkgDocField -> Float
    paramB NameField = 0.9
    -- paramB SynopsisField    = 0.5
    -- paramB DescriptionField = 0.5

    paramFieldWeights :: PkgDocField -> Float
    paramFieldWeights NameField        = 20
    -- paramFieldWeights SynopsisField    = 5
    -- paramFieldWeights DescriptionField = 1


stopWords :: Set Search.Term
stopWords =
  Set.fromList
  ["haskell","library","simple","using","interface","functions",
    "implementation","package","support","'s","based","for","a","and","the",
    "to","of","with","in","an","on","from","that","as","into","by","is",
    "some","which","or","like","your","other","can","at","over","be","it",
    "within","their","this","but","are","get","one","all","you","so","only",
    "now","how","where","when","up","has","been","about","them","then","see",
    "no","do","than","should","out","off","much","if","i","have","also"]
