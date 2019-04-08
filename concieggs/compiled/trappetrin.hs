module Main (main) where

import Data.Char (chr)
import Control.Monad (forM_)

op, ned :: String
op = map chr [9601..9608] ++ " "
ned = reverse op

trappe :: String -> String
trappe = (op ++) . (++ ned)

main :: IO ()
main = fmap lines getContents >>= mapM_ (putStrLn . trappe)
