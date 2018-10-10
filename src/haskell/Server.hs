{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import qualified Data.ByteString.Char8 as BS
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Data.Package
import Search


type Api =
  "search" :> QueryParam "term" String :> Get '[JSON] [Package] :<|>
  Raw


api :: Proxy Api
api = Proxy


run :: [Package] -> IO ()
run packages = do
  let port = 8080
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp packages


mkApp :: [Package] -> IO Application
mkApp = return . serve api . server


server :: [Package] -> Server Api
server packages =
  searchPackages packages :<|>
  serveDirectoryFileServer "./"


searchPackages :: [Package] -> Maybe String -> Handler [Package]
searchPackages packages queryParam =
  case queryParam of
    Just term ->
      return $ Search.perform (BS.pack term) packages
    Nothing -> return packages
