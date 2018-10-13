{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Servant
import System.IO
import Data.Text (Text)

import qualified Token.TypeSig
import qualified Search.Result as SR
import Data.Package
import Search


type Api =
  "search" :> QueryParam "term" String :> Get '[JSON] [SR.Result] :<|>
  Raw


api :: Proxy Api
api = Proxy


run :: [Package] -> IO ()
run packages = do
  let port = 8080
      valueTokens = Token.TypeSig.tokenize packages
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  putStr $ unlines . map show $ Map.toList $ Map.map length valueTokens
  runSettings settings =<< mkApp packages valueTokens


mkApp :: [Package] -> Token.TypeSig.Tokens -> IO Application
mkApp packages valueTokens =
  return . gzip def { gzipFiles = GzipCompress } . serve api $ server packages valueTokens


server :: [Package] -> Token.TypeSig.Tokens -> Server Api
server packages valueTokens =
  searchPackages packages valueTokens :<|>
  serveDirectoryFileServer "./"


searchPackages :: [Package] -> Token.TypeSig.Tokens -> Maybe String -> Handler [SR.Result]
searchPackages packages valueTokens queryParam =
  case queryParam of
    Just term ->
      return $ Search.perform (Text.pack term) packages valueTokens
    Nothing -> return []
