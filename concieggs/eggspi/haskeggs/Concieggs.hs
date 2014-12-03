module Concieggs where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import System.Exit
import System.Process
import Data.List.Utils (split)


-- THE TYPE (just EitherT IO, but might need to change, so kept simple for now)
data ConcieggsM a = ConcieggsM { evalConcieggsM :: IO (Either Int a) }


-- ALL THE INSTANCES
instance Monad ConcieggsM where
  return = ConcieggsM . return . Right

  ConcieggsM io >>= g = ConcieggsM $ do
    x <- io
    case x of
      Left y -> return $ Left y
      Right y -> evalConcieggsM $ g y

instance MonadIO ConcieggsM where
  liftIO = ConcieggsM . fmap Right

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

runIO :: String -> IO (String, ExitCode)
runIO s = case split " " s of
  (prog : args) -> do
    (i, out, err) <- readProcessWithExitCode prog args ""
    return (out, i)
  [] -> return ("", ExitFailure 1)

run :: String -> ConcieggsM String
run prog = do
  (s, c) <- liftIO $ runIO prog
  case c of
    ExitSuccess -> return s
    ExitFailure n -> stop n

runcmd :: String -> ConcieggsM String
runcmd = run . ("runcmd " ++)

runConcieggsM :: ConcieggsM String -> IO ()
runConcieggsM m = do
  x <- evalConcieggsM m
  case x of
    Right s -> putStr s
    Left n -> exitWith $ ExitFailure n
