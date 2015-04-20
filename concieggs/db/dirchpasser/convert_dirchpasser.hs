#!/usr/bin/env runghc
module Main where

import System.Environment
import System.Exit
import System.Directory
import System.FilePath
import Data.List (intercalate)
import Data.List.Utils
import Control.Monad (mapM_)
import Control.Applicative ((<$>))

------------------------------------------------------------

data Token = Word String
           | Comma
           | Period
           | Dot
           | DotDotDot
           | Colon
           | ApostropheStart
           | ApostropheEnd
           | QuotationMarkStart
           | QuotationMarkEnd
           | Newline
           deriving (Show, Eq)

env :: String -> IO String
env k = do
  vm <- lookupEnv k
  case vm of
    Just v -> return v
    Nothing -> exitWith $ ExitFailure 1

onlyTextFiles :: [FilePath] -> [FilePath]
onlyTextFiles = filter (\p -> takeExtension p == ".txt")

largestWord :: [Token] -> Int
largestWord = maximum . map tokenLength
  where tokenLength (Word s) = length s
        tokenLength _ = 0

separatePunctuation :: String -> [Token]
separatePunctuation s
  | endswith "," s = separatePunctuation (init s) ++ [Comma]
  | endswith "..." s = separatePunctuation (init $ init $ init s) ++ [DotDotDot]
  | endswith "." s = separatePunctuation (init s) ++ [Dot]
  | endswith ":" s = separatePunctuation (init s) ++ [Colon]
  | endswith "'" s = separatePunctuation (init s) ++ [ApostropheEnd]
  | startswith "'" s = [ApostropheStart] ++ separatePunctuation (tail s)
  | endswith "\"" s = separatePunctuation (init s) ++ [QuotationMarkEnd]
  | startswith "\"" s = [QuotationMarkStart] ++ separatePunctuation (tail s)
  | otherwise = [Word s]

formatToken :: Token -> String
formatToken t = case t of
  Word w -> "w" ++ w
  Comma -> ","
  Period -> ".."
  Dot -> "."
  DotDotDot -> "..."
  Colon -> ":"
  ApostropheStart -> "'<"
  ApostropheEnd -> "'>"
  QuotationMarkStart -> "\"<"
  QuotationMarkEnd -> "\">"
  Newline -> "n"

------------------------------------------------------------

type Monologsætning = [Token]
type Monologafsnit = [Monologsætning]
type Monolog = [Monologafsnit]

buildMonolog :: FilePath -> FilePath -> IO ()
buildMonolog destPath srcPath = do
  appendFile destPath . formatMonolog . buildMonolog' =<< readFile srcPath

buildMonolog' :: String -> Monolog
buildMonolog' = map (map ((\ts -> if not (null ts) && last ts == Dot
                                  then init ts ++ [Period]
                                  else ts)
                           . concatMap separatePunctuation . split " ")
                     . split "  " . replace "\n" " " . replace ".\n" ".  ")
                . split "\n\n"

formatMonolog :: Monolog -> String
formatMonolog = concatMap (("\n\n\n" ++) . formatMonologafsnit)

formatMonologafsnit :: Monologafsnit -> String
formatMonologafsnit = intercalate "\n\n" . map formatMonologsætning

formatMonologsætning :: Monologsætning -> String
formatMonologsætning = intercalate "\n" . map formatToken

------------------------------------------------------------

-- type Rolle = String
-- type Sketchsætning = [Token]
-- type Sketchafsnit = (Rolle, [Sketchsætning])
-- type Sketch = [Sketchafsnit]

buildSketch :: FilePath -> FilePath -> IO ()
buildSketch = buildMonolog -- not optimal

------------------------------------------------------------

type Verselinje = [Token]
type Vers = [Verselinje]
type Vise = [Vers]

buildVise :: FilePath -> FilePath -> IO ()
buildVise destPath srcPath = do
  appendFile destPath . formatVise . buildVise' =<< readFile srcPath

buildVise' :: String -> Vise
buildVise' = map (map ((++ [Newline]) . concatMap separatePunctuation
                       . split " ")
                  . split "\n")
             . split "\n\n"

formatVise :: Vise -> String
formatVise = concatMap (("\n\n\n" ++) . formatVers)

formatVers :: Vers -> String
formatVers = intercalate "\n\n" . map formatVerselinje

formatVerselinje :: Verselinje -> String
formatVerselinje = intercalate "\n" . map formatToken

------------------------------------------------------------

main :: IO ()
main = do
  writeFile "monologer.txt" ""
  mapM_ (buildMonolog "monologer.txt")
    =<< (map ("monologer/" ++) . onlyTextFiles)
    <$> getDirectoryContents "monologer"

  writeFile "sketcher.txt" ""
  mapM_ (buildSketch "sketcher.txt")
    =<< (map ("sketcher/" ++)
         . onlyTextFiles) <$> getDirectoryContents "sketcher"

  writeFile "viser.txt" ""
  mapM_ (buildVise "viser.txt")
    =<< (map ("viser/" ++)
         . onlyTextFiles) <$> getDirectoryContents "viser"
