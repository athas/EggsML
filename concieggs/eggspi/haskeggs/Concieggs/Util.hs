{-# LANGUAGE OverloadedStrings #-}

module Concieggs.Util where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import Data.Text (Text)
import qualified Data.Text as Text

getCommandArgs :: IO [Text]
getCommandArgs = fmap Text.pack <$> getArgs

getEggsUser :: IO Text
getEggsUser = do
  x <- lookupEnv "EGGS_USER"
  pure (maybe "" Text.pack x)
