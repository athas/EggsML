module Main (main) where

import System.Environment (getArgs)
import Control.Monad
import Control.Applicative
import Concieggs.Monad
import Concieggs.Random


tilfældigTing :: Random String
tilfældigTing =
  choice
  [ "concieggs"
  , "skinke"
  , "katte"
  , "Starbucks"
  , "Pizza Hut"
  , "fastpladelageret"
  ]

tilfældigFørsteperson :: Random String
tilfældigFørsteperson =
  choice
  [ "destrueres"
  , "æder"
  , "fiser højlydt"
  , "galloperer deruda'"
  , "siger nej"
  ]

tilfældigTyper :: Random String
tilfældigTyper =
  choice
  [ "typer"
  , "mønttællere"
  , "bureaukrater"
  , "bankdirektører"
  , "studerende"
  , "algoritmikere"
  ]

tilfældigMådeform :: Random String
tilfældigMådeform =
  choice
  [ "trampe"
  , "bande og svovle"
  , "hoppe og hisse"
  , "maltraktere"
  , "forandre"
  ]

tilfældigKommentar :: String -> String -> Random (ConcieggsM String)
tilfældigKommentar person0 person1 =
  choice
  [ (\s -> "Di" ++ s ++ "! ;-)") <$> getOut (run "tøj")
  , getOut (run "tærteKommentar")
  , getOut (run "sayNo")
  , getOut (run ("nag " ++ person1))
  , getOut $ do
       dummeBavianer <- getOut (run "dummeBavianer")
       hjuleneDrejer <- getOut (run "hjuleneDrejer")
       echo ("Det er jo de " ++ dummeBavianer ++ " der gør at "
             ++ hjuleneDrejer ++ "!")
  ]

main :: IO ()
main = runConcieggsM $ join $ liftRand $ weightedChoice
       [ (hvadErOppe, 1)
       , (menDetVar, 1)
       , (ogSåSagde, 3)
       ]
  where hvadErOppe :: ConcieggsM ()
        hvadErOppe = do
          ting <- liftRand tilfældigTing
          førsteperson <- liftRand tilfældigFørsteperson
          typer <- liftRand tilfældigTyper
          mådeform <- liftRand tilfældigMådeform
          echo ("Hvad er oppe med " ++ ting
                  ++ "?  Det er jo ikke fordi " ++ ting
                  ++ " " ++ førsteperson ++ ", men alligevel kan alle "
                  ++ typer ++ " ikke lade være med at " ++ mådeform ++ "!")

        ogSåSagde :: ConcieggsM ()
        ogSåSagde = do
          brugere <- lines <$> getOut (run "recentlyActive")
          let findPerson = if null brugere
                           then return "concieggs" -- concieggs er der altid.
                           else liftRand $ choice brugere
          person0 <- findPerson
          person1 <- findPerson
          kommentar <- join $ liftRand $ tilfældigKommentar person0 person1
          echo ("Og SÅ sagde " ++ person0 ++ " til " ++ person1 ++ ": \"" ++ kommentar ++ "\"!!!")

        menDetVar :: ConcieggsM ()
        menDetVar = do
          ting <- liftRand tilfældigTing
          echo ("Men det var slet ikke " ++ ting ++ "!  HAHAHAHA!")
