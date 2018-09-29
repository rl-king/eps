{-# LANGUAGE OverloadedStrings #-}

module Main ( main) where

import Control.Exception
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS


-- PACKAGE

data Package = Package
  { packageName :: String
  , versions :: [String]
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
  { moduleName :: String
  , comment :: String
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
  docs <- getPackageDocs packageList
  print packageList
  print docs


readCache :: IO (Maybe [Package])
readCache = do
  file <- BS.readFile "./cache/search.json"
  return (Aeson.decode file :: Maybe [Package])


refreshCache :: IOException -> IO (Maybe [Package])
refreshCache _ = do
  packages <- getPackageList
  return packages


getPackageList :: IO (Maybe [Package])
getPackageList = do
  manager <- Http.newManager TLS.tlsManagerSettings
  request <- Http.parseRequest "https://package.elm-lang.org/search.json"
  response <- Http.httpLbs request manager
  let packages = Aeson.decode (Http.responseBody response) :: Maybe [Package]
  BS.writeFile "./cache/search.json" (encode packages)
  return packages


getPackageDocs :: Maybe ([Package]) -> IO (Maybe [Docs])
getPackageDocs packages =
  case packages of
    Just (hd@(Package n _):_) ->
      do
        manager <- Http.newManager TLS.tlsManagerSettings
        request <- Http.parseRequest (toDocsUrl hd)
        response <- Http.httpLbs request manager
        let docs = Aeson.decode (Http.responseBody response) :: Maybe [Docs]
        BS.writeFile ("./cache/foo.json") (encode docs)
        return docs

    _ ->
      return Nothing


toDocsUrl :: Package -> String
toDocsUrl (Package pName pVersions) =
  "https://package.elm-lang.org/packages/" ++ pName ++ "/" ++ head pVersions ++ "/docs.json"
