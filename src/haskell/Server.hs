{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Map.Strict (Map)
import Network.Wai
import Network.Wai.Handler.Warp
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
      searchIndex = Search.buildIndex packages
      packageMap = Map.fromList $ (\p -> (_pName p, p)) <$> packages
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  Search.indexStats searchIndex
  runSettings settings =<< mkApp searchIndex packageMap


mkApp :: Search.Index -> Map Name Package-> IO Application
mkApp searchIndex packageMap =
  return . serve api $
  server searchIndex packageMap


server :: Search.Index -> Map Name Package -> Server Api
server searchIndex packageMap =
  searchPackages searchIndex packageMap :<|>
  serveDirectoryFileServer "./"


searchPackages :: Search.Index -> Map Name Package -> Maybe Text -> Handler [SR.Result]
searchPackages searchIndex packageMap queryParam =
  case queryParam of
    Just term -> do
      let (info, results) = Search.perform term packageMap searchIndex
      _ <- liftIO $ mapM putStrLn info
      _ <- liftIO $ putStrLn "============"
      return results
    Nothing ->
      return []
