{-# LANGUAGE TypeApplications #-}

module Concieggs.Stateful
  ( Handler
  , statefulMain
  , encodeStateFile
  , decodeStateFile
  , decodeStateFileWithDefault
  ) where

import Control.Monad (join)
import Data.Bifunctor (bimap, first)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Exception (catch, IOException, displayException, SomeException, try)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Foldable (for_)

type Handler s = (s, [Text]) -> IO (s, [Text])

statefulMain :: (FromJSON s, ToJSON s) => FilePath -> IO s -> Handler s -> [Text] -> IO ()
statefulMain stateFilePath initState handler inputs = do
  origState <- decodeStateFileWithDefault stateFilePath initState
  (newState, outputs) <- handler (origState, inputs)
  encodeStateFile stateFilePath newState
  for_ outputs Text.putStrLn

decodeStateFile :: FromJSON a => FilePath -> IO (Either String a)
decodeStateFile = fmap (join . first (displayException @SomeException)) . try . Aeson.eitherDecodeFileStrict'

decodeStateFileWithDefault :: FromJSON a => FilePath -> IO a -> IO a
decodeStateFileWithDefault filePath defaultState = do
  stateMaybe <- decodeStateFile filePath
  case stateMaybe of
    Left _err -> defaultState
    Right a -> pure a

encodeStateFile :: ToJSON a => FilePath -> a -> IO ()
encodeStateFile = Aeson.encodeFile
