module Main
       (main)
where

import Fast.FastParser
import Fast.FastInterpreter

import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
            file : args -> do
              s <- readFile file
              case parseString s of
                Left e -> error $ show e
                Right prog ->
                  case runProg prog args of
                    Left e -> error $ show e
                    Right output -> putStr output
            _ ->
              error "Give me at least a single argument!"
