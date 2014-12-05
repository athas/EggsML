{-# LANGUAGE TupleSections #-}
module Concieggs.Monad where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import System.Exit
import System.Process
import Data.List.Utils (split, endswith)

import Concieggs.Random


-- THE TYPE (just EitherT IO, but might need to change, so kept simple for now)
data ConcieggsM a = ConcieggsM { evalConcieggsM :: IO (Either Int (a, String)) }


-- ALL THE INSTANCES
instance Monad ConcieggsM where
  return a = ConcieggsM $ return $ Right (a, "")

  ConcieggsM io >>= g = ConcieggsM $ do
    x <- io
    case x of
      Left y -> return $ Left y
      Right (y, out) -> fmap (\(t, out') -> (t, out ++ out'))
                        <$> (evalConcieggsM $ g y)

instance MonadIO ConcieggsM where
  liftIO = ConcieggsM . fmap (Right . (, ""))

instance MonadPlus ConcieggsM where
  mzero = stop 1
  mplus m n = ConcieggsM $ do
    x <- evalConcieggsM m
    case x of
      Right _ -> return x
      Left _ -> evalConcieggsM n

instance Functor ConcieggsM where
  fmap = liftM

instance Applicative ConcieggsM where
  pure = return
  (<*>) = ap

instance Alternative ConcieggsM where
  (<|>) = mplus
  empty = mzero


-- THE EXTRA FUNCTIONS
stop :: Int -> ConcieggsM a
stop = ConcieggsM . return . Left

runConcieggsM :: ConcieggsM () -> IO ()
runConcieggsM m = do
  x <- evalConcieggsM m
  case x of
    Right ((), s) -> putStr $ if endswith "\n" s then s else (s ++ "\n")
    Left n -> exitWith $ ExitFailure n

getOut :: ConcieggsM () -> ConcieggsM String
getOut m = do
  x <- liftIO $ evalConcieggsM m
  case x of
    Right (_, out) -> return out
    Left n -> stop n

liftRand :: Random a -> ConcieggsM a
liftRand = liftIO . evalRandIO

echo :: String -> ConcieggsM ()
echo s = ConcieggsM $ return $ Right ((), s)

runIO :: String -> IO (String, ExitCode)
runIO s = case split " " s of
  (prog : args) -> do
    (i, out, err) <- readProcessWithExitCode prog args ""
    return (if endswith "\n" out then init out else out, i)
  [] -> return ("", ExitFailure 1)

run' :: String -> ConcieggsM String
run' prog = do
  (s, c) <- liftIO $ runIO prog
  case c of
    ExitSuccess -> return s
    ExitFailure n -> stop n

run :: String -> ConcieggsM ()
run prog = echo =<< run' prog

runcmd :: String -> ConcieggsM ()
runcmd = run . ("runcmd " ++)


-- SOME OPERATORS
(<++>) :: ConcieggsM [a] -> ConcieggsM [a] -> ConcieggsM [a]
m <++> n = do
  s <- m
  t <- n
  return (s ++ t)

(<++) :: ConcieggsM [a] -> [a] -> ConcieggsM [a]
m <++ t = m <++> pure t

(++>) :: [a] -> ConcieggsM [a] -> ConcieggsM [a]
(++>) = flip (<++)
