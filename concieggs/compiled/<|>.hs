module Main (main) where

-- Mest bare bevis af koncept.  Kommer først til at virke når runcmd exiter med
-- kode ikke 0 når der er en fejl.

import System.Environment (getArgs)
import Control.Applicative ((<|>))
import Concieggs.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
    [first, second] -> runConcieggsM ((runcmd first) <|> (runcmd second))
    _ -> return ()
