{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Main (main) where

import Control.Applicative
import Data.Char
import Data.List (tails, maximumBy, nub, sort)
import Data.Monoid
import Data.Ord
import Data.String
import Data.Text (Text)
import System.Process
import System.Random
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Meaning = Meaning { meaningTo :: Maybe Text
                       , meaningMentionedEggsers :: [Text]
                       , meaningExcitement :: Int
                       , meaningTWSS :: Bool
                       }

deduceMeaning :: Text -> IO Meaning
deduceMeaning s = do
  usernames <- T.lines <$> T.pack <$> readProcess "allAliases" [] ""
  return Meaning { meaningTo = sayingTo s
                 , meaningMentionedEggsers = findMentioned usernames s
                 , meaningExcitement = findExcitement s
                 , meaningTWSS = didSheSaySo s
                 }

sayingTo :: Text -> Maybe Text
sayingTo s
  | not (T.null aft), not (T.null bef), T.all constituent bef = Just bef
  | otherwise = Nothing
  where (bef, aft) = T.span (/=':') s
        constituent = not . isSpace

findMentioned :: [Text] -> Text -> [Text]
findMentioned usernames s =
 case map (nub . sort . (filter (/="og")) . takeWhile possible) $
      tails $
      map (T.dropWhileEnd isPunctuation) $
      T.words s of
   [] -> []
   xs -> maximumBy (comparing length) xs
 where possible w = w `elem` usernames || w == "og"

findExcitement :: Text -> Int
findExcitement s = fst (T.foldl countBangs (0, 0) s) -
                   length (filter ("..." `T.isPrefixOf`) (T.tails s)) * 5
  where countBangs (excitement, 0) '!' = (excitement + 1, 1)
        countBangs (excitement, x) '!' = (excitement + 1 + x * 2, (x + 1) * 2)
        countBangs (excitement, _) _   = (excitement, 0)

didSheSaySo :: Text -> Bool
didSheSaySo s = "Det sagde hun også i går" `T.isInfixOf` s

actionsFromMeaning :: RandomGen g => g -> Meaning -> Text
actionsFromMeaning g meaning = runRandomly g $
                               startMsg <> ", " <>
                               peopleMsg <>
                               ambienceMsg <>
                               "."
  where excited = meaningExcitement meaning
        startMsg
          | Just to <- meaningTo meaning =
            ("stirrer stift på " <> pure to) <|>
            (excited < 2) ==>
            ("går" <|>
             (excited == 0) ==> "lunter"<|>
             "løber" <|>
             "trasker") <>
             " over " <> ("mod"<|>"til") <> " " <> pure to <|>
            ("knurrer" <>
             ((excited < 2) ==> " sagte" <|>
             ((excited > 3) ==> " ophiset")) <>
             " " <> ("ad"<|>"efter"<|>"mod") <> " " <> pure to) <|>
            (excited > 4) ==> "brøler voldsomt mod " <> pure to
          | otherwise =
              (excited < 2) ==>
              ("rejser sig dovent" <|>
               "gaber" <|>
               "kigger sig træt omkring" <|>
               ("kigger sig " <> ("nysgerrigt" <|> "træt") <> " omkring") <|>
               "strækker sig") <|>
              "knurrer lidt" <|>
              "brøler" <|>
              "rejser sig pludseligt" <|>
              "sætter i løb"
        ambienceMsg =
          (excited < 2) ==>
          ("lunter lidt rundt" <|>
           "snuser til luften" <|>
           "dasker beslutsomt til et vandløb" <|>
           "kradser lidt i et træ" <|>
           ("klør sig dovent " <> ("bag øret"<|>"på næsen")) <|>
           "leder efter honning i en hul træstub" <|>
           "slikker sig om munden" <|>
           "slikker sig på næsen" <|>
           "vælter en skraldespand" <|>
           ("klør sig på siden med " <> ("en klo"<|>"det ene "<>("forben"<|>"bagben"))) <|>
           "knurrer lidt målløst" <|>
           "slår en prut" <|>
           "slipper en vind" <|>
           "rejser sig op" <|>
           "gaber dovent" <|>
           "virker pludselig forskrækket" <|>
           ("får færten af " <> ("nogle godter"<|>
                                 "noget bacon"<|>
                                 "hvem ved"<|>
                                 "nogle sure sokker")) <|>
           ("graver" <> (" lidt"<|>"") <> " i en myreture")) <|>
          (excited > 2) ==>
          ("brøler majestætisk" <|>
           "stamper vredt i jorden" <|>
           "ryster voldsomt hovedet fra side til side" <|>
           "vælter et træ")
        peopleMsg
          | mentioned <- meaningMentionedEggsers meaning, not (null mentioned),
            who <- pure $ enumerate mentioned =
            ((excited < 4) ==>
             ((("går en " <> ("omgang"<|>"tur")) <|> "trasker") <>
              " " <> ("rundt om"<|>"omkring"<|>"ved") <>
              " " <> who) <|>
             ("knurrer" <> (" lidt"<>"") <> " mod " <>
              who) <|>
            (excited > 3) ==> ("jager " <> who <> " på flugt") <|>
            (excited > 4) ==> ("rejser sig truende og brølende ved " <> who) <|>
            (excited > 5) ==> ("slår voldsomt ud efter " <> who) <|>
            (excited > 6) ==> ("vælter " <> who <> " omkuld")) <>
            " og "
          | otherwise =
            ambienceMsg <> " og "

enumerate :: [Text] -> Text
enumerate [] = mempty
enumerate [t] = t
enumerate [t1,t2] =
  t1 <> " og " <> t2
enumerate (t:ts) =
  t <> ", " <> enumerate ts

doActions :: Text -> IO ()
doActions = T.putStrLn . ("/me "<>)

main :: IO ()
main = do
  g <- getStdGen
  doActions =<< actionsFromMeaning g <$> (deduceMeaning =<< T.getContents)

data Randomly a = Always a
                | Randomly [Randomly a]

runRandomly :: RandomGen g => g -> Randomly a -> a
runRandomly _ (Always x) = x
runRandomly _ (Randomly []) =
  error "runRandomly: no options"
runRandomly g (Randomly xs) =
  runRandomly g' $ xs !! (i-1)
  where (i, g') = randomR (0, length xs) g

instance Functor Randomly where
  f `fmap` Always x = Always $ f x
  f `fmap` Randomly xs = Randomly (map (fmap f) xs)

instance Applicative Randomly where
  pure = Always
  Always f <*> Always x =
    Always $ f x
  Always f <*> Randomly xs =
    Randomly $ map (fmap f) xs
  Randomly fs <*> Always x =
    Randomly [ f <*> pure x | f <- fs ]
  Randomly fs <*> Randomly xs =
    Randomly [ f' <*> x' | f' <- fs, x' <- xs ]

instance Alternative Randomly where
  empty = Randomly []
  Always x <|> Always y =
    Randomly [Always x, Always y]
  Randomly xs <|> Always y =
    Randomly $ xs ++ [pure y]
  Always x <|> Randomly ys =
    Randomly $ Always x : ys
  Randomly xs <|> Randomly ys =
    Randomly $ xs ++ ys

instance Monoid a => Monoid (Randomly a) where
  mempty = pure mempty
  Always x `mappend` Always y =
    Always $ x `mappend` y
  Always x `mappend` Randomly ys =
    Randomly [ fmap (x `mappend`) y | y <- ys ]
  Randomly xs `mappend` Always y =
    Randomly [ fmap (`mappend` y) x | x <- xs ]
  Randomly xs `mappend` Randomly ys =
    Randomly [ x `mappend` y | x <- xs, y <- ys ]

instance IsString (Randomly Text) where
  fromString = pure . T.pack

infixr 4 ==>
(==>) :: Bool -> Randomly a -> Randomly a
p ==> r
  | p         = r
  | otherwise = empty

