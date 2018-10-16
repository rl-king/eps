{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import qualified Data.Text as Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Servant
import System.IO

import qualified Search.Result as SR
import qualified Search
import Data.Package



type Api =
  "search" :> QueryParam "term" String :> Get '[JSON] [SR.Result] :<|>
  Raw


api :: Proxy Api
api = Proxy


run :: [Package] -> IO ()
run packages = do
  let port = 8080
      searchIndex = Search.index packages
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  Search.info searchIndex
  runSettings settings =<< mkApp packages searchIndex


mkApp :: [Package] -> Search.Index -> IO Application
mkApp packages searchIndex =
  return . gzip def { gzipFiles = GzipCompress } . serve api $
  server packages searchIndex


server :: [Package] -> Search.Index -> Server Api
server packages searchIndex =
  searchPackages searchIndex :<|>
  serveDirectoryFileServer "./"


searchPackages :: Search.Index -> Maybe String -> Handler [SR.Result]
searchPackages searchIndex queryParam =
  case queryParam of
    Just term ->
      return $ Search.perform (Text.pack term) searchIndex
    Nothing ->
      return []
