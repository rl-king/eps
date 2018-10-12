{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import qualified Data.ByteString.Char8 as BS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Servant
import System.IO
import Data.Text (Text, pack)

import Token.Value
import Data.Package
import Search


type Api =
  "search" :> QueryParam "term" String :> Get '[JSON] [Text] :<|>
  Raw


api :: Proxy Api
api = Proxy


run :: [Package] -> IO ()
run packages = do
  let port = 8080
      valueTokens = Token.Value.tokenize packages
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp packages valueTokens


mkApp :: [Package] -> ValueTokens -> IO Application
mkApp packages valueTokens =
  return . gzip def { gzipFiles = GzipCompress } . serve api $ server packages valueTokens


server :: [Package] -> ValueTokens -> Server Api
server packages valueTokens =
  searchPackages packages valueTokens :<|>
  serveDirectoryFileServer "./"


searchPackages :: [Package] -> ValueTokens -> Maybe String -> Handler [Text]
searchPackages packages valueTokens queryParam =
  case queryParam of
    Just term ->
      return $ Search.perform (pack term) packages valueTokens
    Nothing -> return []
