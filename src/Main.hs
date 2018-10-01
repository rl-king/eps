{-# LANGUAGE OverloadedStrings #-}

module Main ( main) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Debug.Trace

import Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as TLS
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Server
import qualified System.IO as IO
import Data.Text (Text)




-- PACKAGE


data Package = Package
  { packageName :: Text
  , summary :: Text
  , versions :: [Text]
  } deriving (Show, Eq)


instance Aeson.FromJSON Package where
  parseJSON =
    Aeson.withObject "Package" $ \v ->
      Package <$> v .: "name" <*> v .: "summary" <*> v .: "versions"


instance Aeson.ToJSON Package where
    toJSON (Package x y z) = object ["name" .= x, "summary" .= y, "versions" .= z]
    toEncoding (Package x y z) = pairs ("name" .= x <> "summary" .= y <> "versions" .= z)


-- DOCS


data Docs = Docs
  { moduleName :: Text
  , comment :: Text
  } deriving (Show)


instance Aeson.FromJSON Docs where
  parseJSON =
    Aeson.withObject "Docs" $ \v ->
      Docs <$> v .: "name" <*> v .: "comment"


instance Aeson.ToJSON Docs where
    toJSON (Docs x y) = object ["name" .= x, "comment" .= y]
    toEncoding (Docs x y) = pairs ("name" .= x <> "comment" .= y)


-- MAIN

main :: IO ()
main = do
  packageList <- catch readCache refreshCache
  -- docs <- sequence $ fmap getPackageDocs (take 4 packageList)
  -- LBS.writeFile ("./cache/docs.json") (encode docs)
  print packageList
  -- server packageList

  let loop = do
        putStr "search term> "
        IO.hFlush IO.stdout
        t <- BS.getLine
        unless (BS.null t) $ do
          putStrLn "Ranked results:"
          let rankedResults = performSearch packageList t

          putStr $ unlines
            [show name | (Package name _ _) <- take 100 rankedResults ]
          loop
  return ()
  loop


readCache :: IO [Package]
readCache = do
  file <- LBS.readFile "./cache/search.json"
  case Aeson.decode file :: Maybe [Package] of
    Just xs -> return xs
    Nothing -> return []


refreshCache :: IOException -> IO [Package]
refreshCache _ = do
  packages <- getPackageList
  case packages of
    Just xs -> return xs
    Nothing -> return []


getPackageList :: IO (Maybe [Package])
getPackageList = do
  response <- request "https://package.elm-lang.org/search.json"
  let packages = Aeson.decode (Http.responseBody response) :: Maybe [Package]
  LBS.writeFile "./cache/search.json" (encode packages)
  return packages


getPackageDocs :: Package -> IO [Docs]
getPackageDocs package = do
  response <- request (toDocsUrl package)
  case Aeson.decode (Http.responseBody response) :: Maybe [Docs] of
    Just xs -> return xs
    Nothing -> return []


toDocsUrl :: Package -> String
toDocsUrl (Package pName _ pVersions) =
  "https://package.elm-lang.org/packages/" ++ Text.unpack pName ++ "/" ++ (Text.unpack . head) pVersions ++ "/docs.json"



-- REQUEST


request :: String -> IO (Http.Response LBS.ByteString)
request path = do
  m <- Http.newManager TLS.tlsManagerSettings
  r <- Http.parseRequest path
  Http.httpLbs r m



-- SERVER


server :: [Package] -> IO ()
server msg =
  Server.quickHttpServe $ site msg


site :: [Package] -> Snap.Snap ()
site packages =
  Snap.ifTop (Snap.writeLBS $ (encode packages)) <|>
  Snap.route [ ("search", searchHandler packages)]


searchHandler :: [Package] -> Snap.Snap ()
searchHandler packages= do
  term <- Snap.getQueryParam "term"
  maybe
    (Snap.writeBS "must specify echo/param in URL")
    (filterPackages packages . TE.decodeUtf8) term


filterPackages :: [Package] -> Text -> Snap.Snap ()
filterPackages packages term =
  Snap.writeLBS $ encode $
  filter (\(Package n _ _) -> flip (>) 1 . length $ Text.splitOn term n) packages



-- SEARCH


performSearch :: [Package] -> BS.ByteString -> [Package]
performSearch packages term =
  let
    inTitle =
      filter (\(Package n _ _) -> byteStringContains term (TE.encodeUtf8 n)) packages

    inSummary =
      filter (\(Package _ s _) -> byteStringContains term (TE.encodeUtf8 s)) packages
  in
    List.nub $ inTitle ++ inSummary


byteStringContains :: BS.ByteString -> BS.ByteString -> Bool
byteStringContains term bs =
  not . BS.null . snd $ BS.breakSubstring term bs
