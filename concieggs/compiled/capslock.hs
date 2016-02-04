module Main(main) where

import Data.Char

main :: IO ()
main = interact $ map capslock
  where capslock c
          | isAlpha c && isLower c = toUpper c
          | isAlpha c && isUpper c = toLower c
          | otherwise              = c
