{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module Mock where

import Control.Monad.Free
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import Data.Aeson as Aeson

import System.Directory
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Data.Package


data FetchF next
    = FetchModules Http.Manager Package next
    | FetchPackages Http.Manager ([Package] -> next)
    deriving (Functor)

type Fetch a =
  Free FetchF a

run :: Fetch a -> IO a
run = foldFree morph
  where
    morph :: FetchF a -> IO a
    morph tf =
      case tf of
        FetchModules m package@Package{_pName, _pVersions} next -> do
          response <- (request m . toLatestVersionDocUrl) package
          createDirectoryIfMissing True cacheDir
          LBS.writeFile (cacheDir ++ "/docs.json") (Http.responseBody response)
          return next
          where
            cacheDir =
              Text.unpack $ Text.intercalate "/" ["./cache", _pName, last _pVersions]
            toLatestVersionDocUrl Package{_pName, _pVersions} =
              Text.unpack $ Text.intercalate "/"
              ["https://package.elm-lang.org/packages", _pName, last _pVersions, "docs.json"]

        FetchPackages m next -> do
          response <- request m "https://package.elm-lang.org/search.json"
          LBS.writeFile "./cache/search.json" (Http.responseBody response)
          case Aeson.eitherDecode (Http.responseBody response) :: Either String [Package] of
            Left err -> error err
            Right packages -> next <$> return packages


fetchPackageList :: Http.Manager -> Fetch [Package]
fetchPackageList m =
  Free (FetchPackages m pure)


fetchModules :: Http.Manager -> Package -> Fetch ()
fetchModules m p =
  Free (FetchModules m p (pure ()))


cachePackages :: IO ()
cachePackages = do
  m <- Http.newManager TLS.tlsManagerSettings
  run $ do
    _ <- traverse (fetchModules m) =<< fetchPackageList m
    return ()


-- REQUEST


request :: Http.Manager -> String -> IO (Http.Response LBS.ByteString)
request m path = do
  r <- Http.parseRequest path
  putStrLn $ "fetching: " ++ path
  Http.httpLbs r m
