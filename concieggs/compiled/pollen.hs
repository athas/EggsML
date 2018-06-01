{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Text.XML.Light
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TEN

url = "http://www.dmi.dk/vejr/services/pollen-rss/"

downloadPollen :: IO String
downloadPollen = Network.HTTP.simpleHTTP (getRequest url) >>= getResponseBody

layout :: [[T.Text]] -> T.Text
layout lls = let len1 = maximum $ map (T.length . head) lls
                 len2 = maximum $ map (T.length . head . tail) lls
                 totlen = len1 + len2 + 1
                 divider = T.append (T.append "\n+" (T.append (T.replicate len1 "-") (T.cons '+' (T.replicate len2 "-")))) "+\n"
                 body = T.intercalate divider  [T.append (T.cons '|' (T.justifyLeft len1 ' ' s1))
                                                         (T.snoc (T.cons '|' (T.justifyRight len2 ' ' s2)) '|')
                                            | [s1, s2] <- lls]
                                          in T.strip $ T.append (T.append divider body) divider

findPollen :: Maybe Element -> Maybe T.Text
findPollen e = e >>= findElement (unqual "item")
                 >>= findElement (unqual "description")
                 >>= return . layout . (map $ T.splitOn ": ")
                            . (filter (\x -> x /= ""))
                            . (map ((T.dropEnd 1) . T.strip))
                            . T.lines . TEN.decodeUtf8 . BS.pack . strContent

printResult :: Maybe T.Text -> IO ()
printResult (Just s) = do
  TIO.putStrLn "Atjuu! Her kommer dagens pollental:"
  let s_lines = T.lines s
      blank_lines = repeat $ T.replicate (maximum $ map T.length s_lines) " "
      comb x y
        | " " `T.isPrefixOf` y = x <> T.tail y
        | otherwise = T.init x <> y
  mapM_ TIO.putStrLn $ zipWith comb (s_lines ++ blank_lines) babe
printResult Nothing = TIO.putStrLn "Ingen pollental!"

babe :: [T.Text]
babe =
  [ "            _"
  , "   .&&&&&& / )"
  , "   .&&&/ \\ |/"
  , "   .&& <,( |\\"
  , "    &&  _/ | )"
  , "    _ ) &._/ /"
  , "  /  )__| .'"
  , " /./| _)_)"
  , "( | \\.--|"
  , " \\|  ) !|"
  , " /| /.__|"
  , "(_// _\\_/______"
  , "  (            )"
  , "   '..____.-'/ |"
  , "    \\  |    (  |"
  , "     \\ /     \\ |"
  , "     / |      )|"
  , "    (  |     / |"
  , "     \\ |      \\|"
  , "      )|"
  , "     / |"
  , "      \\|"
  ]

main :: IO ()
main = downloadPollen >>= (printResult . findPollen . parseXMLDoc)
