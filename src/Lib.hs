{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( eps
  ) where

import Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)


data Package = Package
  { title :: Text.Text
  , versions :: [Text.Text]
  } deriving (Show)


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $ \v ->
      Package <$> v .: "name" <*> v .: "versions"


eps :: IO ()
eps = do
  manager <- Http.newManager tlsManagerSettings
  request <- Http.parseRequest "https://package.elm-lang.org/search.json"
  response <- Http.httpLbs request manager
  print (take 4 <$> Aeson.decode (Http.responseBody response) :: Maybe [Package])
