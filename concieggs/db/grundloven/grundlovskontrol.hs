#!/usr/bin/env runghc
module Main where

import System.Environment (getArgs)
import System.Exit
import Data.List
import Data.List.Utils (split, startswith)


-- Dansk
type Filsti = FilePath


-- Lov
data Lov = Lov [Sektion]
         deriving (Show)

data Sektion = Sektion String [Paragraf]
             deriving (Show)

data Paragraf = Paragraf Integer [Afsnit]
              | ParagrafMedStk Integer [Stk]
              deriving (Show)

data Stk = Stk Integer [Afsnit]
         deriving (Show)

type Afsnit = [Sætning]
type Sætning = [Enhed]
type Enhed = String

læsLovtekst :: Filsti -> IO Lov
læsLovtekst fil = do
  tekst <- readFile fil
  let dele = split "\n\n\n" tekst
  return $ foldl sammenfoldLoven (Lov []) dele

data Teksttype = NySektion String
               | NyParagraf Integer String
               | NyParagrafMedStk Integer
               | NytStk Integer String
               deriving (Show)

teksttype :: String -> Teksttype
teksttype t | startswith "§" t =
  if '\n' `elem` t
  then NyParagraf
       (read $ takeWhile (/= '\n') $ tail t)
       (drop 2 $ dropWhile (/= '\n') t)
  else NyParagrafMedStk $ read $ tail t
            | startswith "Stk. " t = NytStk
                                     (read $ takeWhile (/= '\n') $ drop 5 t)
                                     (drop 2 $ dropWhile (/= '\n') t)
            | otherwise = NySektion t

sammenfoldLoven :: Lov -> String -> Lov
sammenfoldLoven (Lov sektioner) tekstfølge = case teksttype tekstfølge of
  NySektion t -> Lov $ indsætSektion t sektioner
  NyParagraf i p -> Lov $ indsætParagraf i p sektioner
  NyParagrafMedStk i -> Lov $ indsætParagrafMedStk i sektioner
  NytStk i s -> Lov $ indsætStk i s sektioner

indsætSektion :: String -> [Sektion] -> [Sektion]
indsætSektion t sektioner = sektioner ++ [Sektion t []]

indsætParagraf :: Integer -> String -> [Sektion] -> [Sektion]
indsætParagraf i t sektioner = case splitAt (length sektioner - 1) sektioner of
  (sr, [Sektion s paragraffer]) -> sr ++ [Sektion s (indsætParagraf' i t paragraffer)]
  x -> error ("forkert inddata ved paragraf " ++ show i ++ ": " ++ show x)

indsætParagraf' :: Integer -> String -> [Paragraf] -> [Paragraf]
indsætParagraf' i t paragraffer = paragraffer ++ [Paragraf i (afsnit t)]

indsætParagrafMedStk :: Integer -> [Sektion] -> [Sektion]
indsætParagrafMedStk i sektioner = case splitAt (length sektioner - 1) sektioner of
  (sr, [Sektion s paragraffer]) -> sr ++ [Sektion s (indsætParagrafMedStk' i paragraffer)]
  x -> error ("forkert inddata ved paragraf " ++ show i ++ ": " ++ show x)

indsætParagrafMedStk' :: Integer -> [Paragraf] -> [Paragraf]
indsætParagrafMedStk' i paragraffer = paragraffer ++ [ParagrafMedStk i []]

indsætStk :: Integer -> String -> [Sektion] -> [Sektion]
indsætStk i t sektioner = case splitAt (length sektioner - 1) sektioner of
  (sr, [Sektion s paragraffer]) -> sr ++ [Sektion s (indsætStk' i t paragraffer)]
  x -> error ("forkert inddata ved stk. " ++ show i ++ ": " ++ show x)

indsætStk' :: Integer -> String -> [Paragraf] -> [Paragraf]
indsætStk' i t paragraffer = case splitAt (length paragraffer - 1) paragraffer of
  (pr, [ParagrafMedStk pi stkr]) -> pr ++ [ParagrafMedStk pi (indsætStk'' i t stkr)]
  x -> error ("forkert inddata ved stk. " ++ show i ++ ": " ++ show x)

indsætStk'' :: Integer -> String -> [Stk] -> [Stk]
indsætStk'' i t stkr = stkr ++ [Stk i (afsnit t)]

afsnit :: String -> [Afsnit]
afsnit = map sætninger . split "\n\n"
         . reverse . dropWhile (== '\n') . reverse

sætninger :: String -> [Sætning]
sætninger = map (concatMap (split " ") . split "\n")
            . mapIfNotLast (++ ".") . concatMap (split ".  ") . split ".\n"


mapIfNotLast :: (a -> a) -> [a] -> [a]
mapIfNotLast f [] = []
mapIfNotLast f [x] = [x]
mapIfNotLast f (x : xs) = f x : mapIfNotLast f xs

pæn :: Lov -> String
pæn (Lov sektioner) = intercalate "\n\n\n" $ map pænSektion sektioner

pænSektion :: Sektion -> String
pænSektion (Sektion t pr) = t ++ "\n" ++ intercalate "\n\n" (map pænParagraf pr)

pænParagraf :: Paragraf -> String
pænParagraf (Paragraf i a) = "§" ++ show i ++ "\n" ++ show a
pænParagraf (ParagrafMedStk i stkr) = "§" ++ show i ++ "\n" ++ show stkr


sektionAfsnit :: Sektion -> [Afsnit]
sektionAfsnit (Sektion _ pr) = concatMap paragrafAfsnit pr

paragrafAfsnit :: Paragraf -> [Afsnit]
paragrafAfsnit (Paragraf _ ar) = ar
paragrafAfsnit (ParagrafMedStk _ stkr) = concatMap stkAfsnit stkr

stkAfsnit :: Stk -> [Afsnit]
stkAfsnit (Stk _ ar) = ar

markovtekst :: Lov -> String
markovtekst (Lov sektioner) =
  intercalate "\n\n\n"
  $ concatMap (map (intercalate "\n"))
  $ concatMap sektionAfsnit sektioner

alleSætninger :: Lov -> String
alleSætninger (Lov sektioner) =
  intercalate "\n"
  $ concatMap (map (intercalate " "))
  $ concatMap sektionAfsnit sektioner


main :: IO ()
main = do
  argumenter <- getArgs
  case argumenter of
    [lovtekstsfil, action] -> do
      lov <- læsLovtekst lovtekstsfil
      case action of
        "pæn" -> putStrLn $ pæn lov
        "markovtekst" -> putStrLn $ markovtekst lov
        "sætninger" -> putStrLn $ alleSætninger lov
        _ -> exitWith $ ExitFailure 1
    _ -> exitWith $ ExitFailure 1
