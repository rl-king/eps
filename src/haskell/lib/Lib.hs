{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Lib
  ( main
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Control.Monad.STM (atomically)
import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Package
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import Network.Wai.Handler.Warp
import Request
import qualified Search
import qualified Search.Result as Search
import Servant
import qualified System.Environment as Env
import System.IO

-- APP
type AppM
  = ReaderT State Handler

data State
  = State
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
      nt s x = runReaderT x s
  _ <- forkIO $ runIndex state
  runSettings (settings port) . serve api $ hoistServer api (nt state) handlers

-- INDEX
runIndex :: State -> IO ()
runIndex State {_sIndex, _sPackages} = do
  packages <- fetchPackages
  _ <- traverse (atomically . modifyTVar' _sIndex . Search.insert) packages
  _ <-
    atomically $ writeTVar _sPackages $
      Map.fromList (fmap (\p -> (_pName p, p)) packages)
  index <- readTVarIO _sIndex
  Search.indexStats index
  return ()

-- SERVER
type Api
  = "search"
    :> QueryParam "term" Text
      :> Get '[JSON] [Search.Result]
        :<|> Raw

api :: Proxy Api
api =
  Proxy

settings :: Int -> Settings
settings port =
  setPort port $
    setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
      defaultSettings

handlers :: ServerT Api AppM
handlers =
  handleSearch :<|>
    serveDirectoryFileServer "./"

handleSearch :: Maybe Text -> AppM [Search.Result]
handleSearch queryParam = do
  State {_sIndex, _sPackages} <- ask
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
  -- response <- request m "https://package.elm-lang.org/search.json"
  response <- request m "http://localhost:8080/cache/search.json"
  case Aeson.eitherDecode (Http.responseBody response) :: Either String [Package] of
    Left err -> error err
    Right packages -> return packages

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
    toLatestVersionDocUrl Package {_pName, _pVersions} =
      Text.unpack $
        Text.intercalate "/"
          ["http://localhost:8080/cache", _pName, last _pVersions, "docs.json"]

-- ["https://package.elm-lang.org/packages", _pName, last _pVersions, "docs.json"]

-- REQUEST
request :: Http.Manager -> String -> IO (Http.Response LBS.ByteString)
request m path = do
  r <- Http.parseRequest path
  putStrLn $ "fetching: " ++ path
  Http.httpLbs r m
