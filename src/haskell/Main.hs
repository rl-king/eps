{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Exception
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
import qualified Snap.Util.FileServe as FileServe
import Data.Aeson as Aeson

import Data.Package



-- MAIN


main :: IO ()
main = do
  -- Load search.json
  packageList <- catch readPackageList (ignoreException fetchPackagesList)
  LBS.writeFile "./cache/search.json" (encode packageList)

  -- Load all.json
  packageListWithDocs <- catch readPackageDocs
    (ignoreException $ mapM getPackageDocs (take 20 packageList))
  LBS.writeFile "./cache/all.json" (encode packageListWithDocs)

  -- Serve "/" "/search" "/search?term="
  server packageListWithDocs


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
  putStrLn $ "fetching: " ++ path
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
  Snap.ifTop (FileServe.serveFile "./index.html") <|>
  Snap.route
  [ ("search", searchHandler packages)
  , ("search", Snap.writeLBS $ encode $ List.map packageName packages)
  ]


searchHandler :: [Package] -> Snap.Snap ()
searchHandler packages = do
  term <- Snap.getQueryParam "term"
  case term of
    Nothing -> Snap.pass
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
