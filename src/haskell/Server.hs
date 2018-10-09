{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.Trans.Except
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Data.Package


type Api =
  "search" :> QueryParam "term" String :> Get '[JSON] [Package] :<|>
  "search" :> Get '[JSON] [Package] :<|>
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
  searchPackages :<|>
  getPackages packages :<|>
  serveDirectoryWebApp "/foo"


getPackages :: [Package] -> Handler [Package]
getPackages packages =
  return packages


searchPackages :: Maybe String -> Handler [Package]
searchPackages = \ case
  Just _ -> return []
  _ -> throwError $ err404 { errBody = "(╯°□°）╯︵ ┻━┻)." }
