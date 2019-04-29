{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BS
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.XML.Light

data DMIProduct = DMIProduct
  { timestamp :: Integer
  , text_type :: String
  , text_format :: String
  , text :: String
  , varnishURL :: String
  } deriving (Generic, Show)

data DMIResult = DMIResult
  { name :: String
  , language :: String
  , products :: DMIProduct
  } deriving (Generic, Show)

newtype PollenResult =
  PollenResult [DMIResult]
  deriving (Generic, Show)

data Pollental = Pollental
  { location :: String
  , values :: [(String, String)]
  }

instance Ae.FromJSON DMIProduct

instance Ae.FromJSON DMIResult

instance Ae.FromJSON PollenResult

url = "https://www.dmi.dk/dmidk_byvejrWS/rest/texts/forecast/pollen/Danmark"

downloadPollen :: IO BS.ByteString
downloadPollen = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response

fe :: String -> Element -> Maybe Element
fe = findElement . unqual

parsePollen :: Element -> Maybe Pollental
parsePollen e = do
  loc <- fmap strContent (fe "name" e)
  readings <- fe "readings" e
  let values = findChildren (unqual "reading") readings
  fmap
    (Pollental loc)
    (mapM
       (\c -> do
          n <- fmap strContent (fe "name" c)
          v <- fmap strContent (fe "value" c)
          return (n, v))
       values)

findPollen :: Element -> Maybe Pollental
findPollen e = do
  pollen <- fe "pollen_info" e
  regions <- fe "region" pollen
  parsePollen regions

printResult :: Maybe Pollental -> IO ()
printResult (Just (Pollental loc v)) = do
  putStrLn $ "Atjuu! Her kommer dagens pollental for " ++ loc ++ ":"
  mapM_
    (\(ty, amount) -> when (amount /= "-") (putStrLn (ty ++ ": " ++ amount)))
    v
printResult Nothing = putStrLn "Ingen pollental!"

decodeXml :: String -> Maybe Pollental
decodeXml s = do
  e <- parseXMLDoc s
  findPollen e

decodeShit :: BS.ByteString -> Maybe Pollental
decodeShit s = do
  jsonPollen <- Ae.decode s
  case jsonPollen of
    PollenResult [o] -> decodeXml (text (products o))
    _ -> Nothing

main :: IO ()
main = do
  p <- downloadPollen
  printResult (decodeShit p)
