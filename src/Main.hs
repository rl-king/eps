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
  docs <- sequence $ fmap getPackageDocs (take 4 packageList)
  BS.writeFile ("./cache/docs.json") (encode docs)
  print packageList
  print docs


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
  "https://package.elm-lang.org/packages/" ++ pName ++ "/" ++ head pVersions ++ "/docs.json"



-- HTTP


request :: String -> IO (Http.Response BS.ByteString)
request path = do
  m <- Http.newManager TLS.tlsManagerSettings
  r <- Http.parseRequest path
  Http.httpLbs r m
