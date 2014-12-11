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
  , "en æske Klodsmajor"
  , "Barbiedukken"
  , "portrættet af Stig Møller"
  , "hotdogs"
  , "Kapitalisme"
  , "rygeheroin"
  , "Offentlighedsloven"
  , "McDonald's"
  , "Lady Gaga"
  , "charterrejser"
  , "apostroffet i \"Jensen's Bøfhus\"?"
  , "utf-8-katastrofen"
  , "sennepen"
  , "inflationen"
  , "skønhedsidealerne"
  ]

tilfældigFørsteperson :: Random String
tilfældigFørsteperson =
  choice
  [ "destrueres"
  , "æder"
  , "sluger"
  , "gnasker"
  , "griner"
  , "bestemmer det hele"
  , "lugter"
  , "spilder på gulvet"
  , "gør nar ad Dronningen"
  , "pletter på lagnet"
  , "fiser højlydt"
  , "galloperer deruda'"
  , "siger nej"
  , "har slået noget"
  , "arbejder for Illuminat"
  , "spiller høj musik efter midnat"
  , "har tabt sig"
  , "snyder i brætspil"
  ]

tilfældigTyper :: Random String
tilfældigTyper =
  choice
  [ "typerne"
  , "mønttællerne"
  , "bureaukraterne"
  , "bankdirektørerne"
  , "de studerende"
  , "algoritmikerne"
  , "bum-hovederne"
  , "nasserøvene"
  , "miljø-forkæmperne"
  , "feministerne"
  , "blærerøvene"
  , "drukkenboltene"
  , "smedene"
  , "fagforeningsbosserne"
  , "pampervældet"
  , "bohemerne"
  , "Safri Duo-medlemmerne"
  , "i Tivolis Pigegarde"
  , "cirkusartisterne"
  , "revytterne"
  , "de fattige"
  ]

tilfældigMådeform :: Random String
tilfældigMådeform =
  choice
  [ "trampe"
  , "bande og svovle"
  , "hoppe og hisse"
  , "maltraktere"
  , "forandre"
  , "vrisse"
  , "brokke sig"
  , "skamme sig"
  , "vrøvle"
  , "\"trille ærten\""
  , "rumle-skide helt vildt!"
  , "stjæle fra kontoret"
  , "lytte til den nye med Scooter"
  , "spille sygt dum"
  , "spille sygt smart"
  , "være flabet"
  , "bryde ophavsretten"
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
