{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Exception
import Control.Monad (unless)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Server
import qualified System.IO as IO
import Data.Aeson as Aeson
import Data.Text (Text)



-- PACKAGE


data Package = Package
  { packageName :: Text
  , summary :: Text
  , versions :: [Text]
  , docs :: [Module]
  } deriving (Show, Eq)


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $
    \v ->
      Package
      <$> v .: "name"
      <*> v .: "summary"
      <*> v .: "versions"
      <*> v .:? "docs" .!= [] -- Fallback to [] as this is not in search.json


instance Aeson.ToJSON Package where
    toJSON (Package x y z d) =
      object ["name" .= x, "summary" .= y, "versions" .= z, "docs" .= d]
    toEncoding (Package x y z d) =
      pairs ("name" .= x <> "summary" .= y <> "versions" .= z <> "docs" .= d)


-- MODULE


data Module = Module
  { name :: Text
  , comment :: Text
  , customTypes :: [CustomType]
  -- , aliases :: [Alias]
  -- , values :: [Value]
  -- , binops :: [Binop]
  } deriving (Show, Eq)


instance Aeson.FromJSON Module where
  parseJSON =
    Aeson.withObject "Module" $
    \v ->
      Module
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "unions"


instance Aeson.ToJSON Module where
    toJSON (Module x y z) =
      object ["name" .= x, "comment" .= y, "unions" .= z]
    toEncoding (Module x y z) =
      pairs ("name" .= x <> "comment" .= y <> "unions" .= z)


type Name = Text
type Comment = Text
type Arguments = [Text]
type Type = Text


data TypeAlias =
  TypeAlias Name Comment Arguments Type


data CustomType =
  CustomType Name Comment Arguments [(Text, [Type])]
  deriving (Show, Eq)

instance Aeson.FromJSON CustomType where
  parseJSON =
    Aeson.withObject "CustomType" $
    \v ->
      CustomType
      <$> v .: "name"
      <*> v .: "comment"
      <*> v .: "args"
      <*> v .: "cases"

instance Aeson.ToJSON CustomType where
    toJSON (CustomType a b c d) =
      object ["name" .= a, "comment" .= b, "args" .= c, "cases" .= d]
    toEncoding (CustomType a b c d) =
      pairs ("name" .= a <> "comment" .= b <> "args" .= c <> "cases" .= d)

data Value =
  Value Comment Type


data Binop =
  Binop Comment Type


-- data Type
--     = Var Text
--     | Lambda Type Type
--     | Tuple [Type]
--     | Type Text [Type]
--     | Record [(Text, Type)] (Maybe Text)


-- MAIN


main :: IO ()
main = do
  packageList <- catch readPackageList (ignoreException fetchPackagesList)
  LBS.writeFile "./cache/search.json" (encode packageList)
  packageListWithDocs <- catch
    readPackageDocs
    (ignoreException $ mapM getPackageDocs (take 1 packageList))
  LBS.writeFile "./cache/all.json" (encode packageListWithDocs)
  print packageListWithDocs
  server packageListWithDocs

  let loop = do
        putStr "search term> "
        IO.hFlush IO.stdout
        t <- BS.getLine
        unless (BS.null t) $ do
          putStrLn "Ranked results:"
          let rankedResults = performSearch packageList t

          putStr $ unlines
            [show name | (Package name _ _ _) <- rankedResults ]
          loop
  return ()
  loop


-- PACKAGESLIST IO


readPackageList :: IO [Package]
readPackageList = do
  file <- LBS.readFile "./cache/search.json"
  case Aeson.decode file :: Maybe [Package] of
    Just xs -> return xs
    Nothing -> return []


fetchPackagesList :: IO [Package]
fetchPackagesList = do
  response <- request "https://package.elm-lang.org/search.json"
  case Aeson.decode (Http.responseBody response) :: Maybe [Package] of
    Just xs -> return xs
    Nothing -> return []


-- DOCS IO


readPackageDocs :: IO [Package]
readPackageDocs = do
  file <- LBS.readFile "./cache/all.json"
  case Aeson.decode file :: Maybe [Package] of
    Just xs -> return xs
    Nothing -> return []


getPackageDocs :: Package -> IO Package
getPackageDocs package = do
  response <- request (toLatestVersionDocUrl package)
  case Aeson.decode (Http.responseBody response) :: Maybe [Module] of
    Just xs -> return (package {docs = xs})
    Nothing -> return package


toLatestVersionDocUrl :: Package -> String
toLatestVersionDocUrl (Package pName _ pVersions _) =
  "https://package.elm-lang.org/packages/"
  ++ Text.unpack pName
  ++ "/"
  ++ (Text.unpack . head . reverse) pVersions
  ++ "/docs.json"


-- REQUEST


request :: String -> IO (Http.Response LBS.ByteString)
request path = do
  m <- Http.newManager TLS.tlsManagerSettings
  r <- Http.parseRequest path
  Http.httpLbs r m


ignoreException :: a -> IOException -> a
ignoreException =
  const


-- SERVER


server :: [Package] -> IO ()
server msg =
  Server.quickHttpServe $ site msg


site :: [Package] -> Snap.Snap ()
site packages =
  Snap.ifTop (Snap.writeLBS $ (encode packages)) <|>
  Snap.route [ ("search", searchHandler packages)]


searchHandler :: [Package] -> Snap.Snap ()
searchHandler packages = do
  term <- Snap.getQueryParam "term"
  case term of
    Nothing -> (Snap.writeBS "must specify echo/param in URL")
    Just x -> (Snap.writeLBS $ encode . List.map packageName $ performSearch packages x)



-- SEARCH


performSearch :: [Package] -> BS.ByteString -> [Package]
performSearch packages term =
  let
    asMap =
      Map.fromList $ List.map (\p@(Package n _ _ _) -> (n, p)) packages

    rank x check weight =
      if byteStringContains term (TE.encodeUtf8 check) then
        (weight, x)
      else
        (0, x)

    inTitle =
      List.map (\(Package x _ _ _) -> rank x x 1) packages

    inSummary =
      List.map (\(Package x s _ _) -> rank x s 0.5) packages

    merge (r1, a) (r2, _) =
      (r1 + r2, a)

    get acc (_, i) =
      case Map.lookup i asMap of
        Just x -> x : acc
        Nothing -> acc
  in
    List.foldl get [] $
    List.take 10 $
    List.sortOn fst $
    List.filter ((/=) 0 . fst) $
    zipWith merge inTitle inSummary


byteStringContains :: BS.ByteString -> BS.ByteString -> Bool
byteStringContains term bs =
  not . BS.null . snd $ BS.breakSubstring term bs
