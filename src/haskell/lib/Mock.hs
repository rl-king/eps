{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module Mock where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Aeson as Aeson

import System.Directory
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Data.Package


cachePackages :: IO ()
cachePackages = do
  m <- Http.newManager TLS.tlsManagerSettings
  _ <- traverse (fetchModules m) =<< fetchPackageList m
  return ()


fetchPackageList :: Http.Manager -> IO [Package]
fetchPackageList m = do
  response <- request m "https://package.elm-lang.org/search.json"
  LBS.writeFile "./cache/search.json" (Http.responseBody response)
  case Aeson.eitherDecode (Http.responseBody response) :: Either String [Package] of
    Left err -> error err
    Right packages -> return packages



fetchModules :: Http.Manager -> Package -> IO ()
fetchModules m package@Package{_pName, _pVersions}= do
  response <- (request m . toLatestVersionDocUrl) package
  createDirectoryIfMissing True cacheDir
  LBS.writeFile (cacheDir ++ "/docs.json") (Http.responseBody response)
  where
    cacheDir =
      Text.unpack $ Text.intercalate "/" ["./cache", _pName, last _pVersions]
    toLatestVersionDocUrl Package{_pName, _pVersions} =
      Text.unpack $ Text.intercalate "/"
      ["https://package.elm-lang.org/packages", _pName, last _pVersions, "docs.json"]



-- REQUEST


request :: Http.Manager -> String -> IO (Http.Response LBS.ByteString)
request m path = do
  r <- Http.parseRequest path
  putStrLn $ "fetching: " ++ path
  Http.httpLbs r m
