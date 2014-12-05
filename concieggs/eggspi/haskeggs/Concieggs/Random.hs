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
  randomSt,
  randomR,
  choice,
  choiceMay,
  choiceM,
  choiceMMay,
  weightedChoice,
  weightedChoiceMay,
  sample,
  shuffle
  ) where
import Control.Monad
import qualified Control.Monad.Random as CMR
import qualified Data.Random as DR
import qualified Data.Random.Extras as DRE
import Data.Random.Distribution.Uniform
import Data.Word (Word64)

type Random a = CMR.Rand CMR.StdGen a
type RandomT m a = CMR.RandT CMR.StdGen m a

-- | Makes a function that returns a DR.RVar usable by e.g. CMR.Rand.
randomSt :: forall m a . CMR.MonadRandom m => DR.RVar a -> m a
randomSt rvar = DR.runRVar rvar (CMR.getRandom :: m Word64)

-- | Random number in range @a..b@.
randomR :: (CMR.Random a, CMR.MonadRandom m) => a -> a -> m a
randomR a b = CMR.getRandomR (a, b)

-- | Random element from list.
choice :: CMR.MonadRandom m => [a] -> m a
choice = randomSt . DRE.choice

choiceM :: CMR.MonadRandom m => [m a] -> m a
choiceM = join . choice

-- | Nothing if empty list.
choiceMay :: CMR.MonadRandom m => [a] -> Maybe (m a)
choiceMay = liftM randomSt . DRE.safeChoice

choiceMMay :: CMR.MonadRandom m => [m a] -> Maybe (m a)
choiceMMay = fmap join . choiceMay

-- | Random element from weighted list.
weightedChoice :: CMR.MonadRandom m => [(a, Rational)] -> m a
weightedChoice = CMR.fromList

weightedChoiceMay :: CMR.MonadRandom m => [(a, Rational)] -> Maybe (m a)
weightedChoiceMay [] = Nothing
weightedChoiceMay xs = Just (CMR.fromList xs)

-- | Random sample from list.
sample :: CMR.MonadRandom m => Int -> [a] -> m [a]
sample n = randomSt . DRE.sample n

-- | Shuffles list.
shuffle :: CMR.MonadRandom m => [a] -> m [a]
shuffle = randomSt . DRE.shuffle

-- | Runs in IO.
runRandTIO :: Monad m => RandomT m a -> IO (m (a, CMR.StdGen))
runRandTIO x = liftM (CMR.runRandT x) CMR.newStdGen

-- | Evaluates in IO.
evalRandTIO :: Monad m => RandomT m a -> IO (m a)
evalRandTIO x = liftM (CMR.evalRandT x) CMR.newStdGen
