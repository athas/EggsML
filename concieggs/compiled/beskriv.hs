module Main (main) where

import System.Environment (getArgs)
import Data.List (intercalate)
import Control.Monad
import Control.Applicative
import Concieggs.Monad
import Concieggs.Random


data Definity = Definite | Indefinite
              deriving (Show)

phrase :: Definity -> ConcieggsM String
phrase Definite = getOut (run "frase bestemt")
phrase Indefinite = getOut (run "frase ubestemt")

sarcasticResponse :: Random String
sarcasticResponse =
  choice
  [ "Ingenting er lidt ligesom ingenting: ingenting."
  , "Vil du have at jeg skal beskrive noget eller bare brokke mig?"
  , "Det er ligesom en bil, dog med...  Aj, helt ærligt, giv mig nu en ting at beskrive."
  ]

response :: String -> Random (ConcieggsM String)
response thing = do
  choice
    [ pure ("Altså, " ++ thing ++ " er lidt ligesom ")
      <++> phrase Indefinite <++>
      pure "; først tror man at det bare er "
      <++> phrase Definite <++>
      pure ", men så viser det sig at være "
      <++> phrase Indefinite <++>
      pure "!"

    , pure ("Hmm, " ++ thing ++ " er noget i retning af... ")
      <++> phrase Definite <++>
      pure "?"

    , pure "Lidt mindre end "
      <++> phrase Indefinite <++>
      pure (".  Eller større?  Det er ikke sådan med " ++ thing ++ ".")

    , pure ("Jeg ved ikke noget om " ++ thing ++ ", men det lyder som ")
      <++> phrase Indefinite <++>
      pure "."

    , pure ("Du ved vel selv hvad " ++ thing ++ " er?  Men ved du hvad ")
      <++> phrase Definite <++>
      pure " har gang i?"
    ]

main :: IO ()
main = do
  args <- getArgs
  runConcieggsM $ echo =<< case args of
    [] -> return =<< liftRand sarcasticResponse
    _ -> join $ liftRand $ response $ intercalate " " args
