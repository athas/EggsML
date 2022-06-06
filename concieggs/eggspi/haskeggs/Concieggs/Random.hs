{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Concieggs.Random (
  CMR.evalRand,
  CMR.evalRandIO,
  CMR.evalRandT,
  CMR.runRandT,
  CMR.runRand,
  runRandTIO,
  evalRandTIO,
  CMR.mkStdGen,
  Random,
  RandomT,
  randomR,
  choice,
  choiceM,
  weightedChoice,
  weightedChoiceMay,
  ) where
import Control.Monad
import qualified Control.Monad.Random as CMR
import qualified Data.Random as DR
import Data.Random.Distribution.Uniform
import Data.Word (Word64)

type Random a = CMR.Rand CMR.StdGen a
type RandomT m a = CMR.RandT CMR.StdGen m a

-- | Random number in range @a..b@.
randomR :: (CMR.Random a, CMR.MonadRandom m) => a -> a -> m a
randomR a b = CMR.getRandomR (a, b)

-- | Random element from list.
choice :: CMR.MonadRandom m => [a] -> m a
choice = weightedChoice . map (,1)

choiceM :: CMR.MonadRandom m => [m a] -> m a
choiceM = join . choice

-- | Random element from weighted list.
weightedChoice :: CMR.MonadRandom m => [(a, Rational)] -> m a
weightedChoice = CMR.fromList

weightedChoiceMay :: CMR.MonadRandom m => [(a, Rational)] -> Maybe (m a)
weightedChoiceMay [] = Nothing
weightedChoiceMay xs = Just (CMR.fromList xs)

-- | Runs in IO.
runRandTIO :: Monad m => RandomT m a -> IO (m (a, CMR.StdGen))
runRandTIO x = liftM (CMR.runRandT x) CMR.newStdGen

-- | Evaluates in IO.
evalRandTIO :: Monad m => RandomT m a -> IO (m a)
evalRandTIO x = liftM (CMR.evalRandT x) CMR.newStdGen
