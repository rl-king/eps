{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Data.Aeson as Aeson

import Data.Package
import Server



-- MAIN


main :: IO ()
main = do
  -- Load search.json
  packageList <- catch readPackageList (ignoreException fetchPackagesList)
  LBS.writeFile "./cache/search.json" (encode packageList)

  -- Load all.json
  packageListWithDocs <- catch readPackageDocs
    (ignoreException $ mapM getPackageDocs packageList)
  LBS.writeFile "./cache/all.json" (encode packageListWithDocs)

  -- Serve "/" "/search" "/search?term="
  Server.run packageListWithDocs


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
    Just xs -> return (package {modules = xs})
    Nothing -> return package


toLatestVersionDocUrl :: Package -> String
toLatestVersionDocUrl (Package pName _ pVersions _) =
  "https://package.elm-lang.org/packages/"
  ++ Text.unpack pName
  ++ "/"
  ++ (Text.unpack . last) pVersions
  ++ "/docs.json"


-- REQUEST


request :: String -> IO (Http.Response LBS.ByteString)
request path = do
  m <- Http.newManager TLS.tlsManagerSettings
  r <- Http.parseRequest path
  putStrLn $ "fetching: " ++ path
  Http.httpLbs r m


ignoreException :: a -> IOException -> a
ignoreException =
  const
