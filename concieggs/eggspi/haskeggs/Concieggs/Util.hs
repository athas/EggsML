{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Concieggs.Util where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs, lookupEnv)
import Data.Text (Text)
import qualified Data.Text as Text
import IncludeEnv.TH (includeEnv)

getCommandArgs :: IO [Text]
getCommandArgs = fmap Text.pack <$> getArgs

getEggsUser :: IO Text
getEggsUser = getEnvDef "EGGS_USER" ""

getEnvDef :: Text -> Text -> IO Text
getEnvDef var def =
  maybe def Text.pack <$> lookupEnv (Text.unpack var)

dbDir :: FilePath
$(includeEnv "CONCIEGGS_DB_DIR" "dbDir")
