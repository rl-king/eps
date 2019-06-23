{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Lib (main) where

import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, modifyTVar', writeTVar)
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


-- MAIN


main :: IO ()
main = do
  port <- maybe 8080 read <$> Env.lookupEnv "PORT"
  index <- atomically $ newTVar Search.emptyIndex
  packages <- atomically $ newTVar Map.empty
  let state = State index packages
  _ <- forkIO $ runIndex state
  runServer port state



-- INDEX


runIndex :: State -> IO ()
runIndex State{_sIndex, _sPackages} = do
  packages <- fetchPackages
  _ <- traverse (atomically . modifyTVar' _sIndex . Search.insert) packages
  _ <- atomically $ writeTVar _sPackages $
    Map.fromList (fmap (\p -> (_pName p, p)) packages)
  return ()



-- SERVER


type Api =
  "search" :> QueryParam "term" Text :> Get '[JSON] [Search.Result] :<|>
  Raw


api :: Proxy Api
api = Proxy


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


fetchPackages :: IO [Package]
fetchPackages =
  traverse fetchModules =<< fetchPackageList


fetchPackageList :: IO [Package]
fetchPackageList = do
  m <- Http.newManager TLS.tlsManagerSettings
  response <- request m "https://package.elm-lang.org/search.json"
  case Aeson.eitherDecode (Http.responseBody response) :: Either String [Package] of
    Left err -> error err
    Right packages -> return $ take 5 packages


fetchModules :: Package -> IO Package
fetchModules package = do
  m <- Http.newManager TLS.tlsManagerSettings
  response <- (request m . toLatestVersionDocUrl) package
  case Aeson.eitherDecode (Http.responseBody response) :: Either String [Module] of
    Left err ->
      error err
    Right modules ->
      return $ package {_pModules = Map.fromList $ fmap (\ms -> (_mName ms, ms)) modules}
  where
    toLatestVersionDocUrl Package{_pName, _pVersions} =
      Text.unpack $ Text.intercalate "/"
      ["https://package.elm-lang.org/packages", _pName, last _pVersions, "docs.json"]


-- REQUEST


request :: Http.Manager -> String -> IO (Http.Response LBS.ByteString)
request m path = do
  r <- Http.parseRequest path
  putStrLn $ "fetching: " ++ path
  Http.httpLbs r m
