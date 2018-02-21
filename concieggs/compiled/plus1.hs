module Main (main) where

import Data.Ord
import Data.List

main :: IO ()
main = interact transform

transform :: String -> String
transform [] = []
transform s@(c:s') = case filter ((`isPrefixOf` s) . fst) replacements of
                       (needle, replacement) : _ ->
                         replacement ++ transform (drop (length needle) s)
                       _ ->  c : transform s'

replacements :: [(String, String)]
replacements = reverse $ sortBy (comparing (length . fst))
  [("en", "to"),
   ("to", "tre"),
   ("tre", "fire"),
   ("fire", "fem"),
   ("fem", "seks"),
   ("seks", "syv"),
   ("syv", "otte"),
   ("otte", "ni"),
   ("ni", "ti"),
   ("ti", "elleve"),
   ("elleve", "tolv"),
   ("tolv", "tretten"),
   ("tretten", "fjorten"),
   ("fjorten", "femten"),
   ("femten", "seksten"),
   ("seksten", "sytten"),
   ("sytten", "atten"),
   ("atten", "nitten")]
