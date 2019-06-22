{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
-- import Control.Concurrent
-- import Control.Concurrent.STM.TVar
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Data.Aeson as Aeson
import System.Directory
import Data.List

import Data.Package
import Server


-- MAIN


main :: IO ()
main = do
  -- Load search.json
  searchJsonExists <- doesFileExist "./cache/packagelist.json"
  unless searchJsonExists fetchPackagesList
  packageList <- readPackageList

  -- Load all.json
  allJsonExists <- doesFileExist "./cache/packagemodules.json"
  unless allJsonExists (fetchModules packageList)
  modules <- readModules

  let toMap =
        Map.fromList . fmap (\m -> (_mName m, m))
      packagesWithModules =
        take 50 $ zipWith (\ms p -> p {_pModules = toMap ms}) modules packageList

  -- Serve "/" "/search" "/search?term="
  Server.run packagesWithModules


-- PACKAGESLIST


readPackageList :: IO [Package]
readPackageList = do
  file <- LBS.readFile "./cache/packagelist.json"
  case Aeson.eitherDecode file :: Either String [Package] of
    Left err -> error err
    Right packages -> return packages


fetchPackagesList :: IO ()
fetchPackagesList = do
  m <- Http.newManager TLS.tlsManagerSettings
  response <- request m "https://package.elm-lang.org/search.json"
  LBS.writeFile "./cache/packagelist.json" (Http.responseBody response)


-- MODULESLIST


readModules :: IO [[Module]]
readModules = do
  file <- LBS.readFile "./cache/packagemodules.json"
  case Aeson.eitherDecode file :: Either String [[Module]] of
    Left err -> error err
    Right modules -> return modules


fetchModules :: [Package] -> IO ()
fetchModules packages = do
  m <- Http.newManager TLS.tlsManagerSettings
  response <- mapM (request m . toLatestVersionDocUrl) packages
  LBS.writeFile "./cache/packagemodules.json" $
    LBS.concat ["[", LBS.intercalate "," (fmap Http.responseBody response), "]"]


toLatestVersionDocUrl :: Package -> String
toLatestVersionDocUrl (Package pName _ pVersions _) =
  intercalate ""
  ["https://package.elm-lang.org/packages/"
  , Text.unpack pName
  , "/"
  , (Text.unpack . last) pVersions
  , "/docs.json"
  ]


-- REQUEST


request :: Http.Manager -> String -> IO (Http.Response LBS.ByteString)
request m path = do
  r <- Http.parseRequest path
  putStrLn $ "fetching: " ++ path
  Http.httpLbs r m
