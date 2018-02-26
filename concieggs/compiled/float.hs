module Main (main) where

import Numeric.IEEE (succIEEE, predIEEE)
import Data.Char (isSpace)

main :: IO ()
main = interact transform
  where transform [] = []
        transform s@(c:cs)
          | not $ isSpace c,
            (x, s') : _ <- reads s =
              show (frob x) ++ transform s'
          | otherwise = c : transform cs

        frob :: Float -> Float
        frob x
          | round x `mod` 2 == 0 = succIEEE x
          | otherwise            = predIEEE x
