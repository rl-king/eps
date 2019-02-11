{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Servant
import System.IO

import qualified Search.Result as SR
import qualified Search
import Data.Package



type Api =
  "search" :> QueryParam "term" Text :> Get '[JSON] [SR.Result] :<|>
  Raw


api :: Proxy Api
api = Proxy


run :: [Package] -> IO ()
run packages = do
  let port = 8080
      searchIndex = Search.index packages
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  -- Search.info searchIndex
  runSettings settings =<< mkApp searchIndex


mkApp :: Search.Index -> IO Application
mkApp searchIndex =
  return . gzip def { gzipFiles = GzipCompress } . serve api $
  server searchIndex


server :: Search.Index -> Server Api
server searchIndex =
  searchPackages searchIndex :<|>
  serveDirectoryFileServer "./"


searchPackages :: Search.Index -> Maybe Text -> Handler [SR.Result]
searchPackages searchIndex queryParam =
  case queryParam of
    Just term ->
      return $ Search.perform term searchIndex
    Nothing ->
      return []
