module Main (main) where

import Numeric.IEEE (succIEEE, predIEEE)

main :: IO ()
main = interact transform
  where transform [] = []
        transform s =
          case reads s of
            (x, s') : _ ->
              show (frob x) ++ transform s'
            _ -> take 1 s ++ transform (drop 1 s)

        frob :: Float -> Float
        frob x
          | round x `mod` 2 == 0 = succIEEE x
          | otherwise            = predIEEE x
