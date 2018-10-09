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
  "search" :> Get '[JSON] [Package] :<|>
  "search" :> Capture "term" String :> Get '[JSON] [Package]


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
  getPackages packages :<|>
  getItemById


getPackages :: [Package] -> Handler [Package]
getPackages packages =
  return packages


getItemById :: String -> Handler [Package]
getItemById = \ case
  "a" -> return []
  _ -> throwError $ err404 { errBody = "(╯°□°）╯︵ ┻━┻)." }


-- SERVER


-- server :: [Package] -> IO ()
-- server msg =
--   Server.quickHttpServe $ site msg


-- site :: [Package] -> Snap.Snap ()
-- site packages =
--   Snap.ifTop (FileServe.serveFile "./index.html") <|>
--   Snap.route
--   [ ("search", searchHandler packages)
--   , ("search", Snap.writeLBS $ encode $ List.map packageName packages)
--   ]


-- searchHandler :: [Package] -> Snap.Snap ()
-- searchHandler packages = do
--   term <- Snap.getQueryParam "term"
--   case term of
--     Nothing -> Snap.pass
--     Just x -> (Snap.writeLBS $ encode . List.map packageName $ performSearch packages x)
