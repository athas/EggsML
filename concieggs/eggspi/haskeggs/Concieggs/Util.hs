{-# LANGUAGE OverloadedStrings #-}

module Concieggs.Util where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import Data.Text (Text)
import qualified Data.Text as Text

getCommandArgs :: IO [Text]
getCommandArgs = fmap Text.pack <$> getArgs

getEggsUser :: IO Text
getEggsUser = getEnvSafe "EGGS_USER" ""

getEnvDef :: Text -> Text -> IO Text
getEnvDef var def =
  maybe def Text.pack <$> lookupEnv (Text.unpack var)
