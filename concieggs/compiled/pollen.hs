{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP
import Text.XML.Light
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TEN

url = "http://www.dmi.dk/vejr/services/pollen-rss/"

downloadPollen :: IO String
downloadPollen = Network.HTTP.simpleHTTP (getRequest url) >>= getResponseBody

findPollen :: Maybe Element -> Maybe T.Text
findPollen e = e >>= findElement (unqual "item")
                 >>= findElement (unqual "description")
                 >>= return . (T.intercalate "\n")
                            . (filter (\x -> x /= ""))
                            . (map ((T.replace "-" "umåleligt") . (T.dropEnd 1) . T.strip))
                            . T.lines . TEN.decodeUtf8 . BS.pack . strContent

printResult :: Maybe T.Text -> IO ()
printResult (Just s) = TIO.putStrLn "Atjuu!" >> TIO.putStrLn s
printResult Nothing = TIO.putStrLn "Ingen pollental!"

main :: IO ()
main = downloadPollen >>= (printResult . findPollen . parseXMLDoc)
