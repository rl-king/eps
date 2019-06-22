{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar, writeTVar)
import Control.Concurrent (forkIO)
import Control.Monad.STM (atomically)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Aeson as Aeson
import Data.List
import Data.Map.Strict (Map)
import Data.Text (Text)

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import qualified System.Environment as Env
import System.IO

import qualified Search.Result as Search
import qualified Search
import Data.Package



-- STATE


data State =
  State
  { _sIndex :: TVar Search.Index
  , _sPackages :: TVar (Map Name Package)
  }



-- API


type Api =
  "search" :> QueryParam "term" Text :> Get '[JSON] [Search.Result] :<|>
  Raw


api :: Proxy Api
api = Proxy



-- MAIN


main :: IO ()
main = do
  port <- maybe 8080 read <$> Env.lookupEnv "PORT"
  index <- atomically $ newTVar Search.emptyIndex
  packages <- atomically $ newTVar Map.empty
  let state = State index packages
  _ <- forkIO $ runIndex state
  runServer port state


runIndex :: State -> IO ()
runIndex State{_sIndex, _sPackages} = do
  searchJsonExists <- doesFileExist "./cache/packagelist.json"
  unless searchJsonExists fetchPackagesList
  packageList <- readPackageList

  allJsonExists <- doesFileExist "./cache/packagemodules.json"
  unless allJsonExists (fetchModules packageList)
  modules <- readModules

  let packagesWithModules =
        zipWith (\ms p -> p {_pModules = toModulesMap ms}) modules packageList
      packagesAsMap =
        Map.fromList $ (\p -> (_pName p, p)) <$> packagesWithModules
      toModulesMap =
        Map.fromList . fmap (\m -> (_mName m, m))

  _ <- traverse (\p -> atomically $ modifyTVar _sIndex (Search.insert p)) packagesWithModules
  _ <- atomically $ writeTVar _sPackages packagesAsMap
  return ()


-- SERVER


runServer :: Int -> State -> IO ()
runServer port state =
  runSettings settings (serve api $ handlers state)
  where
    settings =
      setPort port $
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
      defaultSettings


handlers :: State -> Server Api
handlers state =
  handleSearch state :<|>
  serveDirectoryFileServer "./"


handleSearch :: State -> Maybe Text -> Handler [Search.Result]
handleSearch State{_sIndex, _sPackages} queryParam = do
  index <- liftIO . atomically $ readTVar _sIndex
  packages <- liftIO . atomically $ readTVar _sPackages
  case queryParam of
    Just term -> do
      let (info, results) = Search.perform term packages index
      _ <- liftIO $ mapM putStrLn info
      _ <- liftIO $ putStrLn "============"
      return results
    Nothing ->
      return []


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
