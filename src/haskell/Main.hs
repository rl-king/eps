{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Data.Aeson as Aeson
import Data.Maybe

import Data.Package
import Server



-- MAIN


main :: IO ()
main = do
  -- Load search.json
  packageList <- catch readPackageList (ignoreException fetchPackagesList)

  -- Load all.json
  packageListWithDocs <- catch readPackageDocs
    (ignoreException $ addPackagesModules (take 20 packageList))

  -- Serve "/" "/search" "/search?term="
  Server.run packageListWithDocs


-- PACKAGESLIST IO


readPackageList :: IO [Package]
readPackageList = do
  file <- LBS.readFile "./cache/search.json"
  return $ fromMaybe [] $ Aeson.decode file


fetchPackagesList :: IO [Package]
fetchPackagesList = do
  response <- request "https://package.elm-lang.org/search.json"
  let packages = fromMaybe [] $ Aeson.decode (Http.responseBody response)
  LBS.writeFile "./cache/search.json" (encode packages)
  return packages


-- DOCS IO


readPackageDocs :: IO [Package]
readPackageDocs = do
  file <- LBS.readFile "./cache/all.json"
  return $ fromMaybe [] $ Aeson.decode file


addPackagesModules :: [Package] -> IO [Package]
addPackagesModules packages = do
  response <- mapM (request . toLatestVersionDocUrl) packages
  let withModules = zipWith addModules response packages
  LBS.writeFile "./cache/all.json" (encode withModules)
  return withModules
  where
    addModules maybeModules package =
      case Aeson.decode (Http.responseBody maybeModules) :: Maybe [Module] of
        Just xs -> package {modules = xs}
        Nothing -> package


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
