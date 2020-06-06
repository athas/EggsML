module Main (main) where

import Data.List (lookup)
import Data.Maybe (fromMaybe)
import System.IO (stdout, stdin, hSetEncoding, utf8)

mapping :: [(Char, String)]
mapping = lower ++ upper ++ special
  where lower =
          zip ['a'..'z'] $ map pure ['𝔞'..'𝔷']
        upper =
          zip ['A'..'Z'] $ map pure ['𝔄'..'ℨ']
        special =
          [('æ', "𝔞𝔢"), ('ø', "𝔬𝔢"), ('å', "𝔞𝔞")]

fraktur :: String -> String
fraktur = concatMap onChar
  where onChar c = fromMaybe [c] $ lookup c mapping

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  interact fraktur
