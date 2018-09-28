{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( eps
  ) where

import Data.Aeson as Aeson
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS


-- DEFINITIONS

data Package = Package
  { packageName :: String
  , versions :: [String]
  } deriving (Show)


data Docs = Docs
  { moduleName :: String
  , comment :: String
  } deriving (Show)


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $ \v ->
      Package <$> v .: "name" <*> v .: "versions"


instance Aeson.FromJSON Docs where
  parseJSON =
    Aeson.withObject "Docs" $ \v ->
      Docs <$> v .: "name" <*> v .: "comment"


-- MAIN

eps :: IO ()
eps = do
  manager <- Http.newManager TLS.tlsManagerSettings
  request <- Http.parseRequest "https://package.elm-lang.org/search.json"
  response <- Http.httpLbs request manager
  getPackage (Aeson.decode (Http.responseBody response) :: Maybe [Package])


getPackage :: Maybe ([Package]) -> IO ()
getPackage packages =
  case packages of
    Nothing ->
      putStrLn "meh"

    Just [] ->
      putStrLn "meh"

    Just (hd:_) ->
      do
        manager <- Http.newManager TLS.tlsManagerSettings
        request <- Http.parseRequest (toDocsUrl hd)
        response <- Http.httpLbs request manager
        print (Aeson.decode (Http.responseBody response) :: Maybe [Docs])



toDocsUrl :: Package -> String
toDocsUrl (Package pName pVersions) =
  "https://package.elm-lang.org/packages/" ++ pName ++ "/" ++ head pVersions ++ "/docs.json"
